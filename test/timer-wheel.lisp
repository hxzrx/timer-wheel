(in-package :timer-wheel-tests)

#+:ignore
(test scheduling
  (let ((wheel (make-wheel))
	(timer (make-timer (lambda (whl tmr)
			     (declare (ignore whl tmr))))))
    ;; It's private...but still good to verify
    (signals unscheduled
      (tw::install-timer wheel timer))

    (is (= 15 (tw::calculate-future-slot 10 5 100)))
    (is (= 5 (tw::calculate-future-slot 90 15 100)))
    (is (= 50 (tw::calculate-future-slot 0 150 100)))

    ;; Test that there's a minimum of 1 tick when calculating slots
    (is (= 11 (tw::calculate-future-slot 10 0 100)))

    (schedule-timer wheel timer :ticks 10)
    (is (= 10 (tw::installed-slot timer)))
    ;; When scheduled in the terminal slot, nothing remains on the timer.
    (is (= 0 (remaining timer)))
    (is (= 1 (length (elt (tw::slots wheel) 10))))
    (uninstall-timer wheel timer)
    (is (= 0 (length (elt (tw::slots wheel) 10))))
    (is (eq 'unscheduled (remaining timer)))

    (schedule-timer wheel timer :ticks 150)
    (is (= 0 (tw::installed-slot timer)))
    (is (= 50 (remaining timer)))))
