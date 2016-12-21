(in-package #:timer-wheel.examples)

(defun test (print-interval-ms end-ms)
  (let* ((counter 0)
	 (wheel (tw:make-wheel))

	 ;; completion indicator
	 (complete-lock (bt:make-lock))
	 (complete-cv (bt:make-condition-variable))

	 ;; Save *standard-output* in case running in slime confuses the outputs.
	 (out *standard-output*)

	 ;; Make sure we've got the bindings visible in the callbacks
	 (counter-timer (tw:make-timer
			 (lambda (wheel timer)
			   (incf counter)
			   (tw:schedule-timer wheel timer
					      :ticks 1)))) 
	 (printer-timer (tw:make-timer
			 (lambda (wheel timer)
			   (format out "Tick: ~D~%" counter)
			   (force-output out)
			   (tw:schedule-timer wheel timer
					      :milliseconds print-interval-ms)))))

    ;; Set up the completion notification
    (tw:schedule-timer wheel
		       (tw:make-timer
			(lambda (wheel timer)
			  (declare (ignore wheel timer))
			  (bt:with-lock-held (complete-lock)
			    (bt:condition-notify complete-cv))))
		       :milliseconds end-ms)

    ;; Start processing, and then shutdown gracefully
    (tw:with-timer-wheel wheel
      (tw:schedule-timer wheel printer-timer :milliseconds print-interval-ms)
      (tw:schedule-timer wheel counter-timer :ticks 1)

      ;; Let the timers go till complete
      (bt:with-lock-held (complete-lock)
	(bt:condition-wait complete-cv complete-lock)))))


;; > (tw.examples:test 500 1500)
;; Tick: 4
;; Tick: 9
;; Tick: 14

;; > (tw.examples:test 100 1000)
;; Tick: 0
;; Tick: 1
;; Tick: 2
;; Tick: 3
;; Tick: 4
;; Tick: 5
;; Tick: 6
;; Tick: 7
;; Tick: 8
;; Tick: 9
