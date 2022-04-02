(ql:quickload :cl-trivial-pool)
(in-package #:timer-wheel.examples)

(defun test-thread-pool ()
  (let* ((wheel (tw:make-wheel :size 10 :resolution 100))
         (pool  (tpool:make-thread-pool))
         (work  (tpool:make-work-item :function (lambda ()
                                                  (format t "~&Work item printing!~%")
                                                  (+ 1 2 3))
                                      :pool pool))
         (timer (tw:make-timer :callback (lambda (wheel timer)
                                           (declare (ignore wheel timer))
                                           (tpool:add-work work pool))
                               :scheduler wheel)))
    (tw:schedule-timer wheel timer)
    (sleep 0.11)
    (let ((result (car (tpool:get-result work))))
      (if (= 6 result)
          (format t "~&Success. Result = ~d!~%" result)
          (format t "~&Fail. Result = ~d!~%" result)))))
