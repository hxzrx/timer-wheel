;;;; timer-wheel.lisp

(in-package #:timer-wheel)

;;; "timer-wheel" goes here. Hacks and glory await!

(defparameter *default-resolution* 100 "milliseconds")
(defparameter *default-size* 100 "slots per wheel") ; 100 * 100 makes a ten-seconds-long period scheduler

(defparameter *expired-epsilon* *default-resolution*
  "For those timers which has exipred time specified,
*expired-epsilon* makes an reasonable tolerate to schedule them.")

(define-condition unscheduled (error)
  ((timer :initarg :timer
	  :reader unscheduled-timer)))


(defclass wheel ()
  ((context      :accessor wheel-context    :initarg :context    :initform nil)
   (timeout-lock :accessor timeout-lock                          :initform (bt:make-lock))
   (thread       :accessor wheel-thread     :initarg :thread     :initform nil)
   (slots        :accessor slots            :initarg :slots      :initform nil)
   (current-slot :accessor current-slot                          :initform 0)
   (resolution   :reader   wheel-resolution :initarg :resolution :initform *default-resolution*)
   (reset        :accessor reset                                 :initform nil)
   (name         :accessor name             :initarg :name       :initform (string (gensym "WHEEL-")))))

(defun inspect-wheel (wheel)
  (with-slots (name context slots resolution reset) wheel
    (format nil "Name: ~d, Context: ~d, Slots: ~d, Resolution: ~d, Reset: ~d"
            name
            context
            (length slots)
            resolution
            reset)))

(defmethod print-object ((wheel wheel) stream)
  (print-unreadable-object (wheel stream :type t :identity t)
    (format stream (inspect-wheel wheel))))

(defun make-wheel (&key (size *default-size*)
		     (resolution *default-resolution*)
                     (name (string (gensym "WHEEL-")))
		     (backend :bt))
  "Make a timer wheel with SIZE slots, with a millisecond RESOLUTION,
and BACKEND of :BT (bordeaux-threads... the only backend)."
  (make-instance 'wheel
		 :slots (make-array size :initial-element nil)
		 :resolution resolution
		 :context (ecase backend
			    (:bt (make-bt-context)))
                 :name name))


(defclass timer () ; 计时器包含三个槽位: 剩余时间, 所属时轮槽位索引, 回调函数
  ((remaining      :accessor remaining      :initarg :remaining  :initform nil)
   (installed-slot :accessor installed-slot :initform nil)      ; 本timer处于wheel哪个时间滴答
   (callback       :accessor callback       :initarg :callback) ; 回调参数用于任务的重复调用
   (result         :accessor result         :initarg :result    :initform nil)   ; 给回调函数一个设置结果的可能, 一般不需要使用
   (scheduled-p    :accessor scheduled-p    :initform nil)     ; 已调度为T, 否则NIL
   ;; 考虑是否有这种任务, 超时后就不再运行: 在调度时先设置超时槽位, 在荷载函数内实现是否运行.
   (timeout-p      :accessor timeout-p      :initform nil)     ; 默认NIL, 若运行时已超时则置为T.
   ;; 状态: 取消, 错误(用于记录未捕捉到的错误, 考虑是否有必要), 等待(不需要这个状态), 已运行, 超时, 完成(不需要, 调度器也不知道)
   (status         :accessor status         :initarg :status    :initform :ok)
   (bindings       :accessor bindings       :initarg :bindings  :initform nil)
   (start          :accessor start          :initarg :start     :initform nil) ; start time of this work, in universal milliseconds
   ;; the 3 slots below are used for periodical timer
   (period         :accessor period         :initarg :period    :initform nil) ; period in ticks, nil for non-periodical timer
   (repeats        :accessor repeats        :initarg :repeats   :initform 1)   ; will be scheduled how many times
   (end            :accessor end            :initarg :end       :initform nil) ; end time of this work, in universal milliseconds
   (name           :accessor name           :initarg :name      :initform (string (gensym "TIMER-")))
   (scheduler      :accessor scheduler      :initarg :scheduler :initform nil)
   )
  (:documentation "
remaining: ticks left to funcall the callback.
installed-slot: the slot index where this timer locates in the wheel's slots.
callback: a function which accepts two parameters: wheel and timer.
start: The start time of the timer, will convert to universal time in milliseconds in make-timer.
       Note that the start slot of the timer means it will begin to run after this time,
         but not mean it should run as soon as the time is fulfilled.
         Think about a timer is created with a very long time before the current time,
         the past runs should be discarded or not?
         There is no perfect solution!
period: the period for the repeatable task, and the period should be exact n-times the resolution of the attached scheduler.
        So, if scheduler is initialized, the period should be a positive integer for periodical tasks,
        or nil for non-periodical tasks.
end: the end time of the timer, will convert to universal time in milliseconds in make-timer.
     for those unbound periodical tasks, this will initialized to a most-positive-fixnum.
scheduler: a scheduler this timer attached to, can be re-attached.
result: sometimes one need the returned value of the callback function,
        and this slot provide a clumsy way to store the value,
        one should set this slot-value manually within the callback's function body.
scheduled-p: if it's been scheduled, this slot will be set to T.
timeout-p: T shows that is has a timeout when it got ran, NOT in use currently.
status: default :OK, but when a timer is uninstalled, the status will be set to :canceled.
bindings: specials bindings, may be useful in threads, NOT in use currently.
"))

(defun inspect-timer (timer)
  (with-slots (name remaining result scheduled-p timeout-p status start period repeats end bindings scheduler) timer
    (format nil "Name: ~d, Remaining: ~d ticks, Scheduled-p: ~d, Timeout-p: ~d, Status: ~s, Start time: ~d, Period: ~d, Repeats: ~d times left, End time: ~d, Bindings: ~d, Result: ~d, Scheduler: ~d"
            name remaining scheduled-p timeout-p status
            (if start (universal-milliseconds->timestamp start) nil)
            (if period (if scheduler
                           (format nil "~d ticks" period)
                           (format nil "~d seconds"(/ period 1000)))
                nil)
            repeats
            (if end (universal-milliseconds->timestamp end) nil)
            bindings
            result
            scheduler)))

(defmethod print-object ((timer timer) stream)
  (print-unreadable-object (timer stream :type t :identity t)
    (format stream (inspect-timer timer))))

(defun make-timer (&key callback scheduler start-time end-time (repeat-times 1 repeat-times-supplied-p)
                     period-in-seconds bindings (name (string (gensym "TIMER-"))))
  "Return a new timer object.
callback: a function that accepts WHEEL and TIMER arguments.
scheduler: a wheel instance this new timer will attach to.
start-time: the earliest time this timer begins to run (not schedule),
            start-time accept 3 types of data:
            1. nil, will schedule immediately;
            2. string, a timestring which can be convert to a universal time, with the machine's timezone if not specified.
               eg. 2022-03-24 16:28:00, 2022-03-24T16:28:00.000+08:00;
            3. an instance of local-time:timestamp.
repeat-times: The times the timer will run, and 1 for non-periodical timer,
              The slot-value will decf by 1 after each run and stop when it get down to 0.
              If repeat-times is not supplied but period-in-seconds is supplied, repeats will try to be inferred.
period-in-seconds: This is the interval value for the periodical timer, and nil for the non-periodical timer.
                   But if scheduler is specified, it will convert to the time ticks with respect to the sceduler's resolution.
                   If scheduler is not specified, it will convert to milliseconds and stored in the period slot also.
                   Note that the timer's period in milliseconds should be a common multiple of the resolution of the scheduler,
                     or else an error will be signaled.
                   If repeat-times >= 2, and period-in-seconds is not supplied,
                     the period will be inferenced from start, end, and repeats.
"
  ;; repeat will always have a positive fixnum value, but the real repeat times will also be constrained by wheel's resolution.
  ;; all slots will be set to reduce calculating in scheduling
  ;; start will be update in schedul-timer if delay-seconds specified
  (check-type period-in-seconds (or null positive-real))
  (check-type start-time    (or null positive-fixnum string local-time:timestamp)) ; may allow universal time in the future
  (check-type repeat-times  (or null positive-fixnum))
  (check-type end-time      (or null positive-fixnum string local-time:timestamp))
  (when (and (null end-time) (null period-in-seconds) repeat-times (> repeat-times 1)) ; cannot infer period so it's meaningless
    (error "Meaningless timer with repeat ~d and period ~d" repeat-times period-in-seconds))
  (let* ((start (cond ((null start-time) (get-current-universal-milliseconds)) ; nil will schedule immediately
                      ((stringp start-time) (timestring->universal-milliseconds start-time))
                      ((typep start-time 'local-time:timestamp) (timestamp->universal-milliseconds start-time))
                      (t (error "Invalid start-time: ~d" start-time))))
         (end (cond ((null end-time) nil)
                    ((stringp end-time) (timestring->universal-milliseconds end-time))
                    ((typep end-time 'local-time:timestamp) (timestamp->universal-milliseconds end-time))
                    (t (error "Invalid end-time: ~d" end-time))))
         (repeats (if (and repeat-times-supplied-p repeat-times)
                      repeat-times
                      (if (and end-time period-in-seconds) ; inference the repeats
                          (round (- end start) (* period-in-seconds +milliseconds-per-second+))
                          (if (and (null end-time) period-in-seconds)
                              most-positive-fixnum ; null end-time with period
                              1 ; end-time without period, or null end-time without period
                              ))))
         (period (if period-in-seconds ; if period specified explicitly
                     (if scheduler
                         (let* ((resolution (wheel-resolution scheduler))
                                (period (multiple-value-list (truncate (/ (* period-in-seconds +milliseconds-per-second+)
                                                                          resolution)))))
                           (if (= (cadr period) 0)
                               (car period)
                               (error "The periods of timer <~d> should be exact n-times of the resolution of the scheduler <~d>"
                                      period-in-seconds (/ resolution 1000))))
                         (* period-in-seconds +milliseconds-per-second+))
                     (if (and scheduler end-time (> repeat-times 1)) ; inference the period
                         (let* ((resolution (wheel-resolution scheduler))
                                (inferred-period-in-ms (/ (- end start) repeat-times))
                                (period (multiple-value-list (truncate (/ inferred-period-in-ms resolution)))))
                           (if (= (cadr period) 0)
                               (car period)
                               (error "The periods of timer <~d> should be exact n-times of the resolution of the scheduler <~d>"
                                      period-in-seconds (/ resolution 1000))))
                         (if (and end-time (> repeat-times 1))
                             (/ (- end start) repeat-times) ; can only infer physic time period
                             nil)
                         ))))
    (when (and start end) (assert (>= end start)))
    (make-instance 'timer
		   :callback callback
                   :period period
                   :start start
                   :repeats repeats
                   :end end
                   :bindings bindings
                   :name name
                   :scheduler scheduler)))

(defun attach-scheduler (timer new-scheduler)
  "Attach a timer to a scheduler and (re)set the period."
  (with-slots ((old-period period) (old-scheduler scheduler)) timer
    (when old-scheduler
      (warn "This timer <~d> has attached to a scheduler <~d>, and it will attach to another scheduler <~d>."
            (name timer) (name old-scheduler) (name new-scheduler)))
    (when old-period
      (with-slots ((new-resolution resolution)) new-scheduler
        (if old-scheduler
            (progn (let* ((period-in-ms (* old-period (wheel-resolution old-scheduler)))
                          (new-period (multiple-value-list (truncate (/ period-in-ms new-resolution)))))
                     (if (= (cadr new-period) 0)
                         (setf (slot-value timer 'period) (car new-period))
                         (error "The periods of timer <~d> should be exact n-times of the resolution <~d> of the scheduler <~d>"
                                (* period-in-ms 1000) (/ new-resolution 1000) (name new-scheduler)))))
            (let* ((new-period (multiple-value-list (truncate (/ old-period new-resolution)))))
              (if (= (cadr new-period) 0)
                  (setf (slot-value timer 'period) (car new-period))
                  (error "The periods of timer <~d> should be exact n-times of the resolution of the scheduler <~d>"
                         old-period (/ new-resolution 1000))))))))
  (setf (slot-value timer 'scheduler) new-scheduler)
  timer)

(defmethod get-real-period ((timer timer))
  "Return the timers period in milliseconds."
  (with-slots (period scheduler) timer
    (when period
      (if scheduler
          (* period (wheel-resolution scheduler))
          period))))

(defgeneric install-timer (wheel timer)
  (:documentation "Add TIMER to the WHEEL schedule."))
(defgeneric uninstall-timer (wheel timer)
  (:documentation "Remove TIMER from the WHEEL schedule."))
(defgeneric tick (wheel)
  (:documentation "Operate one tick of the wheel scedule."))

(defmethod invoke-callback ((wheel wheel) (timer timer))
  "Wrap funcall to callback, and check if it is a periodical timer in the :after method."
  ;; if timeout is sensitive for this timer, the codes will be prefer to add here.
  (lambda ()
    (funcall (callback timer) wheel timer)))

(defmethod invoke-callback :after ((wheel wheel) (timer timer))
  (decf (repeats timer))
  (with-slots (end) timer
    (when (> (repeats timer) 0)
      (if (and end (< (+ end *expired-epsilon*) (get-current-universal-milliseconds)))
          (log:info "The timer has expired: ~d" timer)
          (reinstall-timer wheel timer)))))

(defmethod tick ((wheel wheel))
  "Tick function runs all timers in current slot.
Note that this method does not check repeats of each timer,
so every timers enqueued should make sure they have repeats greater than zero."
  (let (tlist)
    ;; Update the wheel
    (bt:with-lock-held ((timeout-lock wheel)) ; locked so that no timer will be enqueued
      (setf (current-slot wheel) (mod (1+ (current-slot wheel))
				      (length (slots wheel))))
      (rotatef tlist (elt (slots wheel) (current-slot wheel))))
    ;; Then run the timers outside the lock
    (dolist (timer tlist)
      (when (eq :ok (status timer)) ; currently only 2 status, :ok and :canceled
        (if (zerop (remaining timer))
            (funcall (invoke-callback wheel timer))
	    (install-timer wheel timer))))))

(defun calculate-future-slot (current-slot remaining slots)
  (mod (+ current-slot (max 1 remaining )) ;minimum resolution of 1 tick , 最小为1是为了防止立即要执行的任务被安排到下一个周期.
       slots))

(defmethod install-timer :before (wheel timer)
  ;; this check can be moved to the :after method initialize-instance of wheel
  (declare (ignore timer))
  (unless (wheel-thread wheel)
    (initialize-timer-wheel wheel)))

(defmethod install-timer ((wheel wheel) (timer timer))
  "This method enqueues the timer to the slot of the wheel, according to the remaining of the timer.
If this timer will be called within one round of the wheel (from the next slot), calculate it's accurate slot index,
else enqueue the timer to the current slot."
  (unless (scheduled-p timer)
    (error 'unscheduled :timer timer))
  (let ((max-wheel-ticks (length (slots wheel))))
    (if (< (remaining timer) max-wheel-ticks) ; will be called within one round of the wheel
	(bt:with-lock-held ((timeout-lock wheel)) ; 注意这里加了锁
	  (let ((future-slot (calculate-future-slot ; calculate the accurate slot index
			      (current-slot wheel)
			      (remaining timer)
			      (length (slots wheel)))))
	    (setf (remaining timer) 0                ; set to 0, will be called within a round
		  (installed-slot timer) future-slot ;
		  ;; Install the timer at the end of the list
		  ;; This ensures that timer evaluation order is FIFO.
		  ;; TODO: Actually use a FIFO instead of a list
		  (elt (slots wheel) future-slot)    ; use svref to speedup the accessing
		  (append (elt (slots wheel) future-slot) ; append相当于对fifo队列的enqueue操作
			  (list timer)))
	    ;; (push timer (elt (slots wheel) future-slot))
	    ))
	;; The timer needs to be reinstalled later
        ;; 为何没加锁?? 如果在其它线程执行本函数可能会有竞争
	(progn (decf (remaining timer) max-wheel-ticks) ; substract ticks num of a round if the timer cannot run with a round
	       (push timer (elt (slots wheel) (current-slot wheel))) ; enqueue the timer to the current slot
	       (setf (installed-slot timer) (current-slot wheel))))  ; the accurate will be re-caculated in the future
    t))

(defmethod reinstall-timer ((wheel wheel) (timer timer))
  "Exactly the same as install-timer, except some state related slot are set here."
  (setf (slot-value timer 'remaining) (period timer)
        (slot-value timer 'status) :ok
        (slot-value timer 'scheduled-p) t)
  (let ((max-wheel-ticks (length (slots wheel))))
    (if (< (remaining timer) max-wheel-ticks)
        (bt:with-lock-held ((timeout-lock wheel))
          (let ((future-slot (calculate-future-slot
                              (current-slot wheel)
                              (remaining timer)
                              (length (slots wheel)))))
            (setf (remaining timer) 0
                  (installed-slot timer) future-slot
                  ;; Install the timer at the end of the list
                  ;; This ensures that timer evaluation order is FIFO.
                  ;; TODO: Actually use a FIFO instead of a list
                  (elt (slots wheel) future-slot)
                  (append (elt (slots wheel) future-slot)
                          (list timer)))
            ;; (push timer (elt (slots wheel) future-slot))
            ))
        ;; The timer needs to be reinstalled later
        (progn (decf (remaining timer) max-wheel-ticks)
               (push timer (elt (slots wheel) (current-slot wheel)))
               (setf (installed-slot timer) (current-slot wheel))))
    t))

(defmethod uninstall-timer ((wheel wheel) (timer timer))
  (setf (slot-value timer 'status) :canceled
        (slot-value timer 'scheduled-p) nil
        (slot-value timer 'remaining) nil
        (slot-value timer 'installed-slot) nil))

(defmethod calculate-slot-index ((wheel wheel) (timer timer))
  "Calculate the first slot index depends on the timer and the wheel.
Note that the start slot of the timer means it will begin to run after this time,
but not means it should run as soon as the time is fulfilled."
  (with-slots (start repeats period) timer
    (let* ((now (get-current-universal-milliseconds)))
      (if (> start now)
          (truncate (/ (- now start) (wheel-resolution wheel)))
          1))))

(defun schedule-timer (wheel timer &optional delay-seconds)
  "Schedule a timer with an optional delay-seconds.
delay-seconds: The real value of seconds, rounded to the nearest resolution tick.
Return T if the timer is succefully scheduled, and return NIl if it's failed.

delay-seconds is used for some timer that will be sheduled some time later.
For simplicity, if delay-seconds is specified, the start timer of the timer will not take effect.
delay-seconds is usually for the kind of timer that is not well designed with many arguments supplied with make-timer,

If one want to schedule a timer with wall time, make the timer with make-timer and supply a start-time argument,
  then called it with schedule-timer and left delay-seconds unsupplied."
  (assert (or (null delay-seconds)
              (and (realp delay-seconds) (> delay-seconds 0))))
  (assert (> (slot-value timer 'repeats) 0))
  (unless (eq wheel (scheduler timer))
    (attach-scheduler timer wheel))
  (if (and (end timer) (< (+ (end timer) *expired-epsilon*)
                          (get-current-universal-milliseconds)))
      (prog1 nil
        (log:info "Cannot schedule a timer that has already expired. Expire: ~d, Now: ~d"
                  (universal-milliseconds->timestamp (end timer))
                  (local-time:now)))
      (let ((calculated-timeout  (if delay-seconds
                                     (round (* +milliseconds-per-second+ delay-seconds) (wheel-resolution wheel))
                                     (calculate-slot-index wheel timer))))
        (setf (slot-value timer 'remaining) (max 1 calculated-timeout)
              (slot-value timer 'scheduled-p) t)
        (install-timer wheel timer))))

(defun schedule-timer-simply (timer &optional delay-seconds)
  "Schedule a timer with the scheduler it's attached to."
  (if-let (wheel (scheduler timer))
    (schedule-timer wheel timer delay-seconds)
    (prog1 nil
      (log:info "Cannot schedule a timer without a scheduler: ~d" timer))))

(defun initialize-timer-wheel (wheel)
  "Ensure the WHEEL is stopped, then initialize the WHEEL context, and start the WHEEL thread."
  (shutdown-timer-wheel wheel)
  (initialize-timer (wheel-context wheel) ; 由于有要确保时轮处于关闭状态的必要,
		    (wheel-resolution wheel)) ;initialize-timer也要对overrun进行重置
  (setf (wheel-thread wheel)
	(bt:make-thread
	 (lambda ()
	   (manage-timer-wheel wheel))
	 :name "TIMER-WHEEL")))

(defun manage-timer-wheel (wheel) ; 在其中进行wheel初始化判断
  "This is the main entry point of the timer WHEEL thread."
  (loop (wait-for-timeout (wheel-context wheel)) ; 先调用wait-for-timeout是为了校准时间
        (when (reset wheel)
          (return))
        ;; Do the things!
        (tick wheel)))

(defun shutdown-timer-wheel (wheel) ; 应该对未处理任务进行标志
  "Notify the wheel thread to terminate, then wait for it."
  (setf (reset wheel) t)
  (shutdown-context (wheel-context wheel))
  (when (wheel-thread wheel)
    (when (bt:thread-alive-p (wheel-thread wheel))
      (bt:join-thread (wheel-thread wheel)))
    (setf (wheel-thread wheel) nil))
  (setf (reset wheel) nil))


(defmacro with-timer-wheel (wheel &body body)
  "Execute BODY after initializing WHEEL, then
clean up by shutting WHEEL down after leaving the scope."
  `(unwind-protect
	(progn
	  (initialize-timer-wheel ,wheel)
	  ,@body)
     (shutdown-timer-wheel ,wheel)))
