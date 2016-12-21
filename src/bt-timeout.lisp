(in-package #:timer-wheel)

;;; The timeout context when the backend is bordeaux-threads
(defclass bt-timeout-context (timeout-context)
  ((end :accessor context-end
	:initform nil)))

(defun make-bt-context ()
  "Return a data structure for managing ticks with BORDEAUX-THREADS"
  (make-instance 'bt-timeout-context))

(defun current-milliseconds ()
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
	(context-end context) (current-milliseconds)))

(defun shutdown-context (context)
  )

(defun wait-for-timeout (context)
  "This thread maintains a continually updating real-time that it is targetting
  for precise sleeps.  If a sleep cannot be done to reach the target
  time...i.e. we've missed the time getting to this thread, then increment the
  *timeout-overrun* counter, and keep going as though all was well.  I cannot
  think of any specific failure behavior that would be generically ok."
  (let ((new-start (current-milliseconds))
	(new-end (+ (context-end context)
		    (context-resolution context))))
    (setf (context-end context) new-end)
    (let ((this-sleep (/ (- new-end new-start)
			 +milliseconds-per-second+)))
      (if (minusp this-sleep)
	  (progn
	    ;; Document the miss and reset the timeout as though
	    ;; we just ended a perfect sleep.
	    (incf (timeout-overrun context))
	    (setf (context-end context) new-start))

	  ;; At least some amount to sleep, so do it.
	  (sleep this-sleep))
      #+nil
      (format t "Just slept ~A milliseconds~%"
	      (- new-end new-start)))))
