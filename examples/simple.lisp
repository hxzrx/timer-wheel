(in-package #:timer-wheel.examples)

;; tick indicator
(defparameter tick-lock (bt:make-lock))
(defparameter tick-cv (bt:make-condition-variable))

(defun release ()
  (bt:with-lock-held (tick-lock)
    (bt:condition-notify tick-cv)))

(defun wait ()
  (bt:with-lock-held (tick-lock)
    (bt:condition-wait tick-cv tick-lock)))

(defun make-timeout (period-ms)
  (lambda (wheel timer)
    (release)
    (tw:schedule-timer wheel timer :milliseconds period-ms)))

(defun simple (&optional (period-ms 100) (times 10))
  (let* ((wheel (tw:make-wheel 100 100))
	 (timer (tw:make-timer (make-timeout period-ms))))
    
    ;; Start processing, and then shutdown gracefully
    (tw:with-timer-wheel wheel
      (tw:schedule-timer wheel timer :milliseconds period-ms)

      (dotimes (i times)
	(wait)
	(format t "Doing some heavy duty work ~D~%" i)

	;; Try removing force-output...
	(force-output)))))
