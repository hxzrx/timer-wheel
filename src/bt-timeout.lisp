(in-package #:timer-wheel)

;;; The timeout context when the backend is bordeaux-threads
(defclass bt-timeout-context (timeout-context) ; 上下文是属于wheel的
  ((end :accessor context-end :initform nil)))

(defun inspect-bt-timeout-context (context)
  (with-slots (resolution timeout-overrun end) context
    (format nil "Resolution: ~d, End: ~d, Timeout-Run: ~d." resolution end timeout-overrun)))

(defmethod print-object ((context bt-timeout-context) stream)
  (print-unreadable-object (context stream :type t :identity t)
    (format stream (inspect-bt-timeout-context context))))

(defun make-bt-context ()
  "Return a data structure for managing ticks with BORDEAUX-THREADS"
  (make-instance 'bt-timeout-context))

#+:ignore
(defun current-milliseconds () ; wall time has been utilized and this function is deposited
  "Utility function to get the current time in milliseconds."
  (declare (optimize speed))
  (* (get-internal-real-time) ; 注意不是外界ut时间, 而是从系统启动至今的累计时间
     ;; Ensure we're talking milliseconds
     (/ +milliseconds-per-second+
	internal-time-units-per-second)))

(defun initialize-timer (context resolution-milliseconds)
  "Called from the wheel thread."
  ;; Initialize the endpoint
  (setf (context-resolution context) resolution-milliseconds ; 需要重置overrun, 原因见initialize-timer-wheel函数
	(context-end context) (get-current-universal-milliseconds)))

(defun shutdown-context (context)
  (declare (ignore context)))

(defun wait-for-timeout (context)
  "This thread maintains a continually updating real-time that it is targetting
  for precise sleeps.  If a sleep cannot be done to reach the target
  time...i.e. we've missed the time getting to this thread, then increment the
  *timeout-overrun* counter, and keep going as though all was well.  I cannot
  think of any specific failure behavior that would be generically ok."
  (let ((new-start (current-universal-milliseconds))
	(new-end (+ (context-end context)
		    (context-resolution context))))
    (setf (context-end context) new-end)
    (let ((this-sleep (/ (- new-end new-start)
			 +milliseconds-per-second+)))
      ;;(log:info "sleep time: ~,2f seconds!" this-sleep)
      (if (minusp this-sleep)
	  (progn
	    ;; Document the miss and reset the timeout as though
	    ;; we just ended a perfect sleep.
	    (incf (timeout-overrun context)) ; 实际上overrun在本库中并没有其它作用, 可能用于其它的监控
	    (setf (context-end context) new-start))
	  ;; At least some amount to sleep, so do it.
	  (sleep this-sleep))
      #+nil
      (format t "Just slept ~A milliseconds~%"
	      (- new-end new-start)))))
