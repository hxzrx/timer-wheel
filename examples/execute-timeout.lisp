(in-package #:timer-wheel.examples)

(defparameter the-result 'not-computed)

(defparameter cv (bt:make-condition-variable))
(defparameter lock (bt:make-lock))

(defun calc-result (thunk)
  (lambda ()
    (let ((result (funcall thunk)))
      (bt:with-lock-held (lock)
	(if (eq the-result 'not-computed)
	    (progn (setf the-result result)
		   (bt:condition-notify cv))
	    (error "The main thread should be killing me!"))))))

(defun timeout-handler (wheel timer)
  (declare (ignore wheel timer))
  (bt:with-lock-held (lock)
    (setf the-result 'timeout)
    (bt:condition-notify cv)))

(defun execute-timeout (thunk timeout)
  ;; Reset the result for recomputing things
  (setf the-result 'not-computed)
  
  (let* ((wheel (tw:make-wheel))
	 (count 0)
	 (timer (tw:make-timer #'timeout-handler))
	 (counter (tw:make-timer
		   (lambda (whl tmr)
		     (incf count)
		     (tw:schedule-timer whl tmr :ticks 1)))))
    
    ;; Start processing, and then shutdown gracefully
    (tw:with-timer-wheel wheel
      (tw:schedule-timer wheel timer :seconds timeout)
      (tw:schedule-timer wheel counter :ticks 1)
      
      (let ((worker (bt:make-thread (calc-result thunk) :name "Calculating Thread")))

	(bt:with-lock-held (lock)
	  (bt:condition-wait cv lock)
	  (if (eq the-result 'timeout)
	      (progn (bt:destroy-thread worker)
		     (list the-result
			   nil))
	      (list the-result
		    ;; Default resolution of the timer wheel is 100ms
		    (* count 1/10))))))))

;; CL-USER> (timer-wheel.examples:execute-timeout (lambda () (sleep 1) 42) 2)
;; (42 1)
;; CL-USER> (timer-wheel.examples:execute-timeout (lambda () (sleep 1) 42) 0.5)
;; (TIMER-WHEEL.EXAMPLES::TIMEOUT NIL)



