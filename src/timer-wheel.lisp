;;;; timer-wheel.lisp

(in-package #:timer-wheel)

;;; "timer-wheel" goes here. Hacks and glory await!

(defparameter *default-resolution* 100 "milliseconds")
(defparameter *default-size* 100 "slots per wheel")

(define-condition unscheduled (error)
  ((timer :initarg :timer
	  :reader unscheduled-timer)))

(defclass timer () ; 计时器包含三个槽位: 剩余时间, 所属时轮槽位索引, 回调函数
  ((remaining :accessor remaining
	      :initarg :remaining
	      :initform 'unscheduled)
   (installed-slot :accessor installed-slot
		   :initform nil)
   (callback :accessor callback
	     :initarg :callback)))

(defun make-timer (callback)
  "Return a timer object with CALLBACK being
a function that accepts WHEEL and TIMER arguments."
  (make-instance 'timer
		 :callback callback))

(defclass wheel ()
  ((context :accessor wheel-context
	    :initarg :context)
   (timeout-lock :accessor timeout-lock
		 :initform (bt:make-lock))
   (thread :accessor wheel-thread
	   :initarg :thread
	   :initform nil)
   (slots :accessor slots
	  :initarg :slots)
   (current-slot :accessor current-slot
		 :initform 0)
   (resolution :reader wheel-resolution
	       :initarg :resolution
	       :initform *default-resolution*)
   (reset :accessor reset
	  :initform nil)))

(defun make-wheel (&optional
		     (size *default-size*)
		     (resolution *default-resolution*)
		     (backend :bt))
  "Make a timer wheel with SIZE slots, with a millisecond RESOLUTION,
and BACKEND of :BT (bordeaux-threads... the only backend)."
  (make-instance 'wheel
		 :slots (make-array size :initial-element nil)
		 :resolution resolution
		 :context (ecase backend
			    (:bt (make-bt-context)))))

(defgeneric install-timer (wheel timer)
  (:documentation "Add TIMER to the WHEEL schedule."))
(defgeneric uninstall-timer (wheel timer)
  (:documentation "Remove TIMER from the WHEEL schedule."))
(defgeneric tick (wheel)
  (:documentation "Operate one tick of the wheel scedule."))

(defmethod tick ((wheel wheel))
  (let (tlist)
    ;; Update the wheel
    (bt:with-lock-held ((timeout-lock wheel)) ; 加锁是为了防止向里面添加计时器
      (setf (current-slot wheel) (mod (1+ (current-slot wheel))
				      (length (slots wheel))))
      (rotatef tlist (elt (slots wheel) (current-slot wheel)))) ; 交换tlist和当前槽位内容, 交换后对应槽位为空

    ;; Then run the timers outside the lock
    (dolist (timer tlist)
      (if (zerop (remaining timer))
	  (funcall (callback timer) wheel timer) ; 计时器剩余事件为零就执行回调
	  (install-timer wheel timer)))))        ; 否则重新安装计时器.

(defmethod install-timer :before (wheel timer)   ; 实际上没必要每次都调用:before, 一次调用即可
  (declare (ignore timer))
  (unless (wheel-thread wheel)
    (initialize-timer-wheel wheel)))

(defun calculate-future-slot (current-slot remaining slots)
  (mod (+ current-slot
	  (max 1 ;minimum resolution of 1 tick , 最小为1是为了防止立即要执行的任务被安排到下一个周期.
	       remaining ))
       slots))

(defmethod install-timer ((wheel wheel) (timer timer))
  (when (eq (remaining timer) 'unscheduled)
    (error 'unscheduled :timer timer))
  (let ((max-wheel-ticks (length (slots wheel))))
    (if (< (remaining timer) max-wheel-ticks) ; 表示在一个周期内能执行的任务(即1秒内能轮到)
	(bt:with-lock-held ((timeout-lock wheel)) ; 注意这里加了锁
	  (let ((future-slot (calculate-future-slot ; 计算此计时器落在哪个槽位
			      (current-slot wheel)
			      (remaining timer)
			      (length (slots wheel)))))
	    (setf (remaining timer) 0                ; 将剩余时间设置为0, 表示下次轮到对应滴答就执行此计时器.
		  (installed-slot timer) future-slot ; 设置计时器的槽位

		  ;; Install the timer at the end of the list
		  ;; This ensures that timer evaluation order is FIFO.
		  ;; TODO: Actually use a FIFO instead of a list
		  (elt (slots wheel) future-slot)    ; 向时轮对应的滴答槽位添加本计时器任务.
		  (append (elt (slots wheel) future-slot) ; append相当于对fifo队列的enqueue操作
			  (list timer)))
	    ;; (push timer (elt (slots wheel) future-slot))
	    ))
	;; The timer needs to be reinstalled later
        ;; 为何没加锁??
	(progn (decf (remaining timer) max-wheel-ticks) ; 如果在一个周期内不能执行此任务.
	       (push timer (elt (slots wheel) (current-slot wheel))) ; 随便讲此任务放到当前滴答, 而不去计算它真正属于哪个槽位
	       (setf (installed-slot timer) (current-slot wheel))))  ; 这样可减少槽位索引计算, 反正此计时器需要重新安装.
    t))

(defmethod uninstall-timer ((wheel wheel) (timer timer)) ; 本方法可以进一步优化, 只需将计时任务设置一个状态以标识是否取消
  (when (installed-slot timer)
    (bt:with-lock-held ((timeout-lock wheel))
      ;; Check again in case something else already uninstalled the timer.
      (when (installed-slot timer)
	(setf (elt (slots wheel) (installed-slot timer)) ; 可用svref替代elt来优化
	      (remove timer
		      (elt (slots wheel) (installed-slot timer))
		      :test #'eq)
	      (remaining timer) 'unscheduled
	      (installed-slot timer) nil)))))

(defun schedule-timer (wheel timer &key
				     (ticks nil ticks-p)
				     (milliseconds nil milliseconds-p)
				     (seconds nil seconds-p))
  "Schedule a timer with one of
  :ticks - The number of resolution steps per (wheel-resolution wheel), minimum
           of 1 tick.
  :milliseconds - The integer number of milliseconds worth of ticks, must be
                  (and (plusp milliseconds)
                       (zerop (mod milliseconds (wheel-resolution wheel)))).
  :seconds - The real value of seconds, rounded to the nearest resolution tick.

In all cases, the timeout will elapse in no more than
   (* calculated-quantity-of-ticks resolution) milliseconds.

The keyword arguments are checked in this order: ticks, milliseconds, seconds
for valid values."
  (let ((calculated-timeout (cond
			      (ticks-p
			       ticks)
			      (milliseconds-p
			       (the integer (/ milliseconds
					       (wheel-resolution wheel))))
			      (seconds-p
			       (round (/ (* +milliseconds-per-second+ seconds)
					 (wheel-resolution wheel)))))))
    (assert (plusp calculated-timeout))
    (setf (remaining timer) calculated-timeout)
    (install-timer wheel timer)))

(defun initialize-timer-wheel (wheel)
  "Ensure the WHEEL is stopped, then initialize the WHEEL
context, and start the WHEEL thread."
  (shutdown-timer-wheel wheel)
  (initialize-timer (wheel-context wheel) ; 由于有要确保时轮处于关闭状态的必要,
		    (wheel-resolution wheel)) ;initialize-timer也要对overrun进行重置
  (setf (wheel-thread wheel)
	(bt:make-thread
	 (lambda ()
	   (manage-timer-wheel wheel))
	 :name "timer-wheel")))

(defun manage-timer-wheel (wheel)
  "This is the main entry point of the timer WHEEL thread."
  (loop
     (wait-for-timeout (wheel-context wheel)) ; 先调用wait-for-timeout是为了校准时间
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
