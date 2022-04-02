(in-package #:timer-wheel.examples)

(defun test (print-interval-sec end-sec)
  (let* ((counter 0)
	 (wheel (tw:make-wheel :size 100 :resolution 100))

	 ;; completion indicator
	 (complete-lock (bt:make-lock))
	 (complete-cv (bt:make-condition-variable))

	 ;; Save *standard-output* in case running in slime confuses the outputs.
	 (out *standard-output*)

	 ;; Make sure we've got the bindings visible in the callbacks
	 (counter-timer (tw:make-timer
			 :callback(lambda (wheel timer)
			            (incf counter)
			            (tw:schedule-timer wheel timer 0.1))
                         :scheduler wheel))
	 (printer-timer (tw:make-timer
			 :callback (lambda (wheel timer)
			             (format out "~&Tick: ~D~%" counter)
			             (force-output out)
			             (tw:schedule-timer wheel timer print-interval-sec))
                         :scheduler wheel)))

    ;; Set up the completion notification
    (tw:schedule-timer wheel
		       (tw:make-timer
			:callback (lambda (wheel timer)
			            (declare (ignore wheel timer))
			            (bt:with-lock-held (complete-lock)
			              (bt:condition-notify complete-cv)))
                        :scheduler wheel)
		        end-sec)

    ;; Start processing, and then shutdown gracefully
    (tw:with-timer-wheel wheel
      (tw:schedule-timer wheel printer-timer print-interval-sec)
      (tw:schedule-timer wheel counter-timer 0.1)

      ;; Let the timers go till complete
      (bt:with-lock-held (complete-lock)
	(bt:condition-wait complete-cv complete-lock)))))


;; > (tw.examples:test 0.5 1.5)
;; Tick: 4
;; Tick: 9
;; Tick: 14

;; > (tw.examples:test 0.1 1)
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
