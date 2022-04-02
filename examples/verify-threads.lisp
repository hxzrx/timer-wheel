;;;; Keep an atomic counter structure, and increase it atomically in the callback of the timer.
;;;; Invoke tw:schedule-timer in threads.
;;;; Check the counter finally.

(in-package #:timer-wheel.examples)

(defparameter *atomic-counter* (tw::make-atomic 0))

(defparameter *verify-wheel* (tw:make-wheel))

(defun gen-timer ()
  (tw:make-timer :callback (lambda (wheel timer)
                             (declare (ignore wheel timer))
                             (tw::atomic-incf (tw::atomic-place *atomic-counter*)))
                 :scheduler *verify-wheel*))

(defun verify-scheduler-threads (threads-num)
  "Schedule timer in threads, check if some timers lost or not."
  (defparameter *atomic-counter* (tw::make-atomic 0))
  (format t "~&Wait a second to get the result...~%")
  (dotimes (i threads-num)
    (bt:make-thread #'(lambda ()
                        (tw:schedule-timer *verify-wheel* (gen-timer)))))
  (sleep (* threads-num 0.0001))
  (if (= threads-num (tw::atomic-place *atomic-counter*))
      (format t "~&Scheduling in threads PASSED!~%")
      (format t "~&scheduling in threads FAILED!~%")))

(verify-scheduler-threads 10000)
