;;;; In this file, two very simple timers were created to test the speed of the wheel.
;;;; The times were made beforehand and the callbacks almost did nothing and thus the tests reflected the scheduling rate.
;;;; The results showed that this wheel can schedule times at the rate of about 2.5M per second.

(in-package #:timer-wheel.examples)

;;; to test this, one should comment the same line below in schedule-timer functin in src/timer-wheel.lisp
;;; (assert (> (slot-value timer 'repeats) 0))

(defparameter *counter* (list 0))
(defparameter *perform-wheel-1* (make-wheel :size 100 :resolution 100))
(defparameter *perform-wheel-2* (make-wheel :size 50  :resolution 50))

(defparameter *perform-timer-simple-1* (make-timer :callback (lambda (wheel timer)
                                                             (declare (ignore wheel timer))
                                                             t)
                                                   :scheduler *perform-wheel-1*))

(defparameter *perform-timer-simple-2* (make-timer :callback (lambda (wheel timer)
                                                             (declare (ignore wheel timer))
                                                             t)
                                                   :scheduler *perform-wheel-2*))

(defparameter *perform-timer-counter-1* (make-timer :callback (lambda (wheel timer)
                                                              (declare (ignore wheel timer))
                                                              (incf (car *counter*)))
                                                    :scheduler *perform-wheel-1*))

(defparameter *perform-timer-counter-2* (make-timer :callback (lambda (wheel timer)
                                                              (declare (ignore wheel timer))
                                                              (incf (car *counter*)))
                                                  :scheduler *perform-wheel-2*))

(defun simple-schedule-perform (wheel timer n)
  (defparameter *counter* (list 0))
  (time (dotimes (i (expt 10 n))
          (schedule-timer wheel timer))))

#|
(simple-schedule-perform *perform-wheel-1* *perform-timer-simple-1* 8)
Evaluation took:
  37.412 seconds of real time
  57.026913 seconds of total run time (56.990463 user, 0.036450 system)
  [ Run times consist of 0.099 seconds GC time, and 56.928 seconds non-GC time. ]
  152.43% CPU
  96,974,120,329 processor cycles
  8,495,636,304 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-1* *perform-timer-counter-1* 8)
Evaluation took:
  34.560 seconds of real time
  54.747443 seconds of total run time (54.735273 user, 0.012170 system)
  [ Run times consist of 0.088 seconds GC time, and 54.660 seconds non-GC time. ]
  158.41% CPU
  89,579,726,802 processor cycles
  8,563,940,256 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-2* *perform-timer-simple-2* 8)
Evaluation took:
  33.888 seconds of real time
  52.268775 seconds of total run time (52.252775 user, 0.016000 system)
  [ Run times consist of 0.055 seconds GC time, and 52.214 seconds non-GC time. ]
  154.24% CPU
  87,833,711,327 processor cycles
  8,521,453,712 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-2* *perform-timer-counter-2* 8)
Evaluation took:
  33.788 seconds of real time
  52.136143 seconds of total run time (52.120156 user, 0.015987 system)
  [ Run times consist of 0.063 seconds GC time, and 52.074 seconds non-GC time. ]
  154.30% CPU
  87,575,646,372 processor cycles
  8,520,422,992 bytes consed
|#
