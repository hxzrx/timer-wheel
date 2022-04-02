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

(defun make-timeout (period-sec)
  (lambda (wheel timer)
    (release)
    (tw:schedule-timer wheel timer period-sec)))

(defun simple (&optional (period-sec 0.1) (times 10))
  (let* ((wheel (tw:make-wheel :size 100 :resolution 100))
	 (timer (tw:make-timer :callback (make-timeout period-sec)
                               :scheduler wheel
                               :period-in-seconds period-sec)))
    ;; Start processing, and then shutdown gracefully
    (tw:with-timer-wheel wheel
      (tw:schedule-timer wheel timer)
      (dotimes (i times)
	(wait)
	(format t "~&Doing some heavy duty work ~D~%" i)
	;; Try removing force-output...
	(force-output)))))
