(in-package #:timer-wheel)

;;; The timeout context when the backend is bordeaux-threads
(defclass bt-timeout-context (timeout-context)
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

(defun current-milliseconds () ; wall time has been utilized and this function is deposited
  "Utility function to get the current time in milliseconds."
  (declare (optimize speed))
  (* (get-internal-real-time)
     ;; Ensure we're talking milliseconds
     (/ +milliseconds-per-second+
	internal-time-units-per-second)))

(defun initialize-timer (context resolution-milliseconds)
  "Called from the wheel thread."
  ;; Initialize the endpoint
  (setf (context-resolution context) resolution-milliseconds
	(context-end context) (get-current-universal-milliseconds)))

(defun shutdown-context (context)
  (declare (ignore context)))

(defun wait-for-timeout (context)
  "This thread maintains a continually updating real-time that it is targetting
  for precise sleeps.  If a sleep cannot be done to reach the target
  time...i.e. we've missed the time getting to this thread, then increment the
  *timeout-overrun* counter, and keep going as though all was well.  I cannot
  think of any specific failure behavior that would be generically ok."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((new-start (the fixnum (current-universal-milliseconds)))
	(new-end (the fixnum (+ (the fixnum (context-end context))
		                (the fixnum (context-resolution context))))))
    (setf (context-end context) new-end)
    (let ((sleep-sec (the fixnum (- new-end new-start))))
      ;;(log:info "sleep time: ~,2f seconds!" this-sleep)
      (if (minusp sleep-sec)
	  (progn
	    ;; Document the miss and reset the timeout as though
	    ;; we just ended a perfect sleep.
	    (incf (the fixnum (timeout-overrun context)))
	    (setf (context-end context) new-start))
	  ;; At least some amount to sleep, so do it.
	  (sleep (the ratio (/ sleep-sec +milliseconds-per-second+))))
      #+nil
      (format t "Just slept ~A milliseconds~%"
	      (- new-end new-start)))))
