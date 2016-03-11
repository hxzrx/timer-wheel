(in-package #:timer-wheel)

;;; TODO: move this to its own file
(defconstant +milliseconds-per-second+ 1000)
(defclass timeout-context ()
  ())
;;; End TODO

;;; The timeout context when the backend is bordeaux-threads
(defclass bt-timeout-context (timeout-context)
  ((timeout-lock :accessor timeout-lock
		 :initform (bt:make-lock))
   (timeout-condition-variable :accessor timeout-condition-variable
			       :initform (bt:make-condition-variable))
   (timeout-interrupter-thread :accessor timeout-interrupter-thread
			       :initform nil)
   (reset :accessor reset
	  :initform nil)
   (timeout-overrun :accessor timeout-overrun
		    :initform 0)))

(defun make-context ()
  (make-instance 'bt-timeout-context))

(defun shutdown-context (context)
  (when (timeout-interrupter-thread context)
    (when (bt:thread-alive-p (timeout-interrupter-thread context))
      (bt:join-thread (timeout-interrupter-thread context)))
    (setf (timeout-interrupter-thread context) nil)))

;; (defvar *timeout-lock* (bt:make-lock))
;; (defvar *timeout-condition-variable* (bt:make-condition-variable))
;; (defvar *timeout-interrupter-thread* nil)
;; (defvar *wheel-thread* nil)
;; (defvar *reset* nil)

(define-condition timeout ()
  ())

(defun current-milliseconds ()
  "Utility function to get the current time in milliseconds."
  (declare (optimize speed))
  (* (get-internal-real-time)
     ;; Ensure we're talking milliseconds
     (/ +milliseconds-per-second+
	internal-time-units-per-second)))

(defun timeout-interrupter (context resolution-milliseconds)
  "The interrupter thread enters here.  This thread maintains a
  continually updating real-time that it is targetting for precise
  sleeps.  If a sleep cannot be done to reach the target
  time...i.e. we've missed the time getting to this thread, then
  increment the *timeout-overrun* counter, and keep going as though
  all was well.  I cannot think of any specific failure behavior that
  would be generically ok."
  (setf (timeout-interrupter-thread context)
	(bt:current-thread))
  (let* ((start (current-milliseconds))
	 (end (+ start resolution-milliseconds)))
    (loop
       (let ((new-start (current-milliseconds))
	     (new-end (+ end resolution-milliseconds)))
	 (setf end new-end)
	 (let ((this-sleep (/ (- new-end new-start)
			      +milliseconds-per-second+)))
	   (if (minusp this-sleep)
	       (progn
		 ;; Document the miss and reset the timeout as though
		 ;; we just ended a perfect sleep.
		 (incf (timeout-overrun context))
		 (setf end new-start))

	       ;; At least some amount to sleep, so do it.
	       (sleep this-sleep))
	   #+nil
	   (format t "Just slept ~A milliseconds~%"
		   (- new-end new-start)))
	 
	 (bt:with-lock-held ((timeout-lock context))
	   (bt:condition-notify (timeout-condition-variable context)))
	 (when (reset context)
	   (return))))))

#|
(defun interrupt ()
  (setf *timeout-interrupter-thread*
	(bt:make-thread (lambda ()
			  (timeout-interrupter 500)))))
|#

(defun initialize-timer (context resolution)
  "Called from the wheel thread."
  (setf (timeout-interrupter-thread context)
	(bt:make-thread
	 (lambda ()
	   (timeout-interrupter context resolution))
	 :name "timeout-interrupter")))

(defun wait-for-timeout (context)
  "The wheel thread calls this to wait for the timeout to elapse,
  returning :timeout...or returning a timer object to be scheduled or
  unscheduled."
  (bt:with-lock-held ((timeout-lock context))
    (bt:condition-wait (timeout-condition-variable context)
		       (timeout-lock context))))
