;;;; timer-wheel.lisp

(in-package #:timer-wheel)

;;; "timer-wheel" goes here. Hacks and glory await!

(defparameter *default-resolution* 100)
(defparameter *default-size* 100)

(define-condition unscheduled (error)
  ((timer :initarg :timer
	  :reader unscheduled-timer)))

(defclass timer ()
  ((remaining :accessor remaining
	      :initarg :remaining
	      :initform 'unscheduled)
   (installed-slot :accessor installed-slot
		   :initform 0)
   (callback :accessor callback
	     :initarg :callback)))
(defun make-timer (callback)
  (make-instance 'timer
		 :callback callback))

(defclass wheel ()
  ((context :accessor wheel-context
	    :initarg :context)
   (thread :accessor wheel-thread
	   :initarg :thread
	   :initform nil)
   (slots :accessor slots
	  :initarg :slots)
   (current-slot :accessor current-slot
		 :initform 0)
   (resolution :accessor wheel-resolution
	       :initarg :resolution
	       :initform *default-resolution*)))
(defun make-wheel (&optional
		     (size *default-size*)
		     (resolution *default-resolution*))
  (make-instance 'wheel
		 :slots (make-array size :initial-element nil)
		 :resolution resolution
		 :context (make-context)))

(defgeneric install-timer (wheel timer))
(defgeneric uninstall-timer (wheel timer))
(defgeneric tick (wheel))

(defmethod tick ((wheel wheel))
  (let (tlist)
    ;; Update the wheel
    (bt:with-lock-held ((timeout-lock (wheel-context wheel)))
      (setf (current-slot wheel) (mod (1+ (current-slot wheel))
				      (length (slots wheel))))
      (rotatef tlist (elt (slots wheel) (current-slot wheel))))

    ;; Then run the timers outside the lock
    (dolist (timer tlist)
      (if (zerop (remaining timer))
	  (funcall (callback timer))
	  (install-timer wheel timer)))))

(defmethod install-timer :before (wheel timer)
  (unless (wheel-thread wheel)
    (initialize-timer-wheel wheel)))

(defun calculate-future-slot (current-slot remaining slots)
  (mod (+ current-slot
	  (max 1 ;minimum resolution of 1 tick
	       remaining ))
       slots))

(defmethod install-timer ((wheel wheel) (timer timer))
  (when (eq (remaining timer) 'unscheduled)
    (error 'unscheduled :timer timer))
  (let ((max-wheel-ticks (length (slots wheel))))
    (if (< (remaining timer) max-wheel-ticks)
	(bt:with-lock-held ((timeout-lock (wheel-context wheel)))
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
	       (push timer (elt (slots wheel) (current-slot wheel)))))
    t))

(defmethod uninstall-timer ((wheel wheel) (timer timer))
  (bt:with-lock-held ((timeout-lock (wheel-context wheel)))
    (setf (elt (slots wheel) (installed-slot timer))
	  (remove timer
		  (elt (slots wheel) (installed-slot timer))
		  :test #'eq)
	  (remaining timer) 'unscheduled)))

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
  (shutdown-timer-wheel wheel)
  (initialize-timer (wheel-context wheel)
		    (wheel-resolution wheel))
  (setf (wheel-thread wheel)
	(bt:make-thread
	 (lambda ()
	   (manage-timer-wheel wheel))
	 :name "timer-wheel")))

(defun manage-timer-wheel (wheel)
  (loop
     (wait-for-timeout (wheel-context wheel))
     (when (reset (wheel-context wheel))
       (return))
     ;; Do the things!
     (tick wheel)))

(defun shutdown-timer-wheel (wheel)
  (setf (reset (wheel-context wheel)) t)

  (shutdown-context (wheel-context wheel))
  
  (when (wheel-thread wheel)
    (when (bt:thread-alive-p (wheel-thread wheel))
      (bt:with-lock-held ((timeout-lock (wheel-context wheel)))
	(bt:condition-notify (timeout-condition-variable (wheel-context wheel))))
      (bt:join-thread (wheel-thread wheel)))
    (setf (wheel-thread wheel) nil))

  (setf (reset (wheel-context wheel)) nil))


(defmacro with-timer-wheel (wheel &body body)
  `(unwind-protect
	(progn
	  (initialize-timer-wheel ,wheel)
	  ,@body)
     (shutdown-timer-wheel ,wheel)))
