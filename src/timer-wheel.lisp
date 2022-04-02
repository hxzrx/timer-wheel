;;;; timer-wheel.lisp

(in-package #:timer-wheel)

;;; "timer-wheel" goes here. Hacks and glory await!

(defparameter *default-resolution* 100 "milliseconds")
(defparameter *default-size* 100 "slots per wheel") ; 100 * 100 makes a ten-seconds-long period scheduler

(defparameter *expired-epsilon* *default-resolution*
  "For those timers which has exipred time specified,
*expired-epsilon* makes an reasonable tolerate to schedule them.")

(defvar *wheel-list* nil
  "A list to keep all wheels.")

(define-condition unscheduled (error)
  ((timer :initarg :timer
	  :reader unscheduled-timer)))


(defclass wheel ()
  ((context      :accessor wheel-context    :initarg :context    :initform nil)
   (thread       :accessor wheel-thread     :initarg :thread     :initform nil)
   (slots        :accessor slots            :initarg :slots      :initform nil)
   (current-slot :accessor current-slot                          :initform (make-atomic 0))
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
		 :slots (make-array size :initial-contents (loop for i below size collect (make-queue 100)))
		 :resolution resolution
		 :context (ecase backend
			    (:bt (make-bt-context)))
                 :name name))

(defmethod initialize-instance :after ((wheel wheel) &key &allow-other-keys)
  (push wheel *wheel-list*))

(defclass timer ()
  ((remaining      :accessor remaining      :initarg :remaining  :initform nil)
   (installed-slot :accessor installed-slot :initform nil)
   (callback       :accessor callback       :initarg :callback)
   (result         :accessor result         :initarg :result    :initform nil)
   (scheduled-p    :accessor scheduled-p    :initform nil)
   (timeout-p      :accessor timeout-p      :initform nil)
   (status         :accessor status         :initarg :status    :initform :ok)
   (bindings       :accessor bindings       :initarg :bindings  :initform nil)
   (start          :accessor start          :initarg :start     :initform nil)
   (end            :accessor end            :initarg :end       :initform nil)
   (period         :accessor period         :initarg :period    :initform nil)
   (repeats        :accessor repeats        :initarg :repeats   :initform 1)
   (name           :accessor name           :initarg :name      :initform (string (gensym "TIMER-")))
   (scheduler      :accessor scheduler      :initarg :scheduler :initform nil))
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
  (with-slots (name remaining installed-slot result scheduled-p timeout-p status start period repeats end bindings scheduler) timer
    (format nil "Name: ~d, Remaining: ~d ticks, Scheduled-p: ~d, Installed to slot: ~d, Timeout-p: ~d, Status: ~s, Start time: ~d, Period: ~d, Repeats: ~d times left, End time: ~d, Bindings: ~d, Result: ~d, Scheduler: ~d"
            name remaining scheduled-p installed-slot timeout-p status
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
                     the period will be inferenced from start, end, and repeats."
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
                     (if (and scheduler end-time repeat-times (> repeat-times 1)) ; inference the period
                         (let* ((resolution (wheel-resolution scheduler))
                                (inferred-period-in-ms (/ (- end start) repeat-times))
                                (period (multiple-value-list (truncate (/ inferred-period-in-ms resolution)))))
                           (if (= (cadr period) 0)
                               (car period)
                               (error "The periods of timer <~d> should be exact n-times of the resolution of the scheduler <~d>"
                                      period-in-seconds (/ resolution 1000))))
                         (if (and end-time repeat-times (> repeat-times 1))
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
(defgeneric reinstall-timer (wheel timer)
  (:documentation "Add TIMER to the WHEEL schedule again."))
(defgeneric uninstall-timer (wheel timer)
  (:documentation "Remove TIMER from the WHEEL schedule."))
(defgeneric invoke-callback (wheel timer)
  (:documentation "Wrap funcall to callback, and check if it is a periodical timer in the :after method."))

(defmethod invoke-callback ((wheel wheel) (timer timer))
  "Wrap funcall to callback, the purpose of this method is to check and then reinstall a periodical timer."
  ;; if timeout is sensitive for this timer, the related codes can be added here.
  (lambda ()
    (funcall (callback timer) wheel timer)))

(defmethod invoke-callback :after ((wheel wheel) (timer timer))
  (decf (repeats timer))
  (with-slots (end) timer
    (when (> (repeats timer) 0)
      (if (and end (< (+ end *expired-epsilon*) (get-current-universal-milliseconds)))
          (log:info "The timer has expired: ~d" timer)
          (reinstall-timer wheel timer)))))

(defun tick (wheel)
  "Tick function runs all timers in current slot.
Note that this method does not check repeats of the timers,
so every timers enqueued should make sure they have repeats greater than zero,
or they will run once even if their repeats less than zero."
  (let ((slot-queue nil)
        (new-queue (make-queue 100))
        (current-slot-atomic (current-slot wheel)))
    ;; Update the wheel
    (atomic-set (atomic-place current-slot-atomic)
                (mod (1+ (atomic-place current-slot-atomic)) (length (slots wheel))))
    (setf slot-queue (atomic-exchange (svref (slots wheel) (atomic-place current-slot-atomic)) new-queue))
    ;;(dolist (timer tlist)
    (loop for timer = (dequeue slot-queue)
          while timer
          do (when (eq :ok (status timer)) ; currently only 2 status, :ok and :canceled
               (if (zerop (remaining timer))
                   (funcall (invoke-callback wheel timer))
	           (install-timer wheel timer))))))

(defun calculate-future-slot (current-slot remaining slots)
  ;; minimum resolution of 1 tick to make sure an urgent task runs in the next wheel period
  (mod (+ current-slot (max 1 remaining ))
       slots))

(defmethod install-timer :before (wheel timer)
  ;; This check can be moved to the :after method initialize-instance of wheel, or before manage-timer-wheel's loop.
  ;; But it's reasonable to check here to make sure the wheel will continue if the thread exits unexpectly.
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
	(let ((future-slot (calculate-future-slot ; calculate the accurate slot index
			    (atomic-place (current-slot wheel))
			    (remaining timer)
			    (length (slots wheel)))))
	  (setf (remaining timer) 0                ; set to 0, will be called within a round
		(installed-slot timer) future-slot)
	  (enqueue timer (svref (slots wheel) future-slot))) ; enqueue is thread safe
	;; The timer needs to be reinstalled later
	(progn (decf (remaining timer) max-wheel-ticks) ; substract ticks num of a round if the timer cannot run with a round
	       (enqueue timer (svref (slots wheel) (atomic-place (current-slot wheel)))) ; enqueue the timer to the current slot
	       (setf (installed-slot timer) (atomic-place (current-slot wheel))))) ; the accurate will be re-caculated in the future
    t))

(defmethod reinstall-timer ((wheel wheel) (timer timer))
  "Exactly the same as install-timer, except some state related slot are set here."
  (setf (slot-value timer 'remaining) (period timer)
        (slot-value timer 'status) :ok
        (slot-value timer 'scheduled-p) t)
  (let ((max-wheel-ticks (length (slots wheel))))
    (if (< (remaining timer) max-wheel-ticks)
        (let ((future-slot (calculate-future-slot
                            (atomic-place (current-slot wheel))
                            (remaining timer)
                            (length (slots wheel)))))
          (setf (remaining timer) 0
                (installed-slot timer) future-slot)
          (enqueue timer (svref (slots wheel) future-slot)))
        (progn (decf (remaining timer) max-wheel-ticks)
               (enqueue timer (svref (slots wheel) (atomic-place (current-slot wheel))))
               (setf (installed-slot timer) (atomic-place (current-slot wheel)))))
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
      (let ((calculated-timeout (if delay-seconds
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
  (initialize-timer (wheel-context wheel)
		    (wheel-resolution wheel))
  (setf (wheel-thread wheel)
	(bt:make-thread
	 (lambda ()
	   (manage-timer-wheel wheel))
	 :name "TIMER-WHEEL")))

(defun manage-timer-wheel (wheel)
  "This is the main entry point of the timer WHEEL thread."
  (loop (wait-for-timeout (wheel-context wheel)) ; invoke wait-for-timeout to adjust the time
        (when (reset wheel)
          (return))
        ;; Do the things!
        (tick wheel)))

(defun shutdown-timer-wheel (wheel)
  "Notify the wheel thread to terminate, then wait for it."
  (setf (reset wheel) t)
  (shutdown-context (wheel-context wheel))
  (when (wheel-thread wheel)
    (when (bt:thread-alive-p (wheel-thread wheel))
      (bt:join-thread (wheel-thread wheel)))
    (setf (wheel-thread wheel) nil)
    (loop for tick-slot across (slots wheel)
          do (loop for timer = (dequeue tick-slot)
                   while timer
                   do (uninstall-timer wheel timer))))
  (setf (reset wheel) nil))

(defmacro with-timer-wheel (wheel &body body)
  "Execute BODY after initializing WHEEL, then clean up by shutting WHEEL down after leaving the scope."
  (let ((wheel$ (gensym))) ; avoid eval wheel twice if wheel is some form like (make-wheel)
    `(let ((,wheel$ ,wheel))
       (unwind-protect
	    (progn
	      (initialize-timer-wheel ,wheel$)
	      ,@body)
         (shutdown-timer-wheel ,wheel$)))))

(defmacro with-timeout ((wheel timeout &optional (scheduler (gensym)) (timer (gensym))) &body body)
  "Encapsule BODY into a timer and schedule it with TIMEOUT seconds and return the timer object.
TIMEOUT is a positive real number in seconds.
SCHEDULER and TIMER are symbols which can be used in BODY as the returned timer's callback's arguments"
  ;; (defparameter *wheel* (make-wheel))
  ;; (with-timeout (*wheel* 1) (format t "with timeout!~%"))
  ;; (with-timeout (*wheel* 1 schedul tim) (format t "Scheduler: ~d, timer: ~d.~%" (name schedul) (name tim)))
  ;; (with-timeout ((make-wheel) 0.00001 schedul tim) (format t "Scheduler: ~d, timer: ~d.~%" (name schedul) (name tim)))
  (let ((wheel$ (gensym)))
    `(let* ((,wheel$ ,wheel)
            (fn (lambda (,scheduler ,timer)
                  (declare (ignorable ,scheduler ,timer))
                  ,@body))
            (new-timer (make-timer :callback fn :scheduler ,wheel$)))
       (schedule-timer ,wheel$ new-timer ,timeout)
       new-timer)))
