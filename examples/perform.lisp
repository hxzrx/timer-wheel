;;;; In this file, two very simple timers were created to test the speed of the wheel.
;;;; The times were made beforehand and the callbacks almost did nothing and thus the tests reflected the scheduling rate.
;;;; The results showed that this wheel can schedule at the rate of about 90M times per second.

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
(simple-schedule-perform *perform-wheel-1* *perform-timer-simple-1* 9)
Evaluation took:
  10.304 seconds of real time
  10.838952 seconds of total run time (10.838952 user, 0.000000 system)
  105.19% CPU
  26,702,323,126 processor cycles
  2,749,312 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-1* *perform-timer-counter-1* 9)
Evaluation took:
  10.676 seconds of real time
  11.208157 seconds of total run time (11.208157 user, 0.000000 system)
  104.98% CPU
  27,671,311,751 processor cycles
  2,390,784 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-2* *perform-timer-simple-2* 9)
  Evaluation took:
  11.160 seconds of real time
  11.758799 seconds of total run time (11.758799 user, 0.000000 system)
  105.37% CPU
  28,928,845,226 processor cycles
  17,602,416 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-2* *perform-timer-counter-2* 9)
Evaluation took:
  10.456 seconds of real time
  10.926709 seconds of total run time (10.926709 user, 0.000000 system)
  104.50% CPU
  27,104,451,880 processor cycles
  2,746,640 bytes consed
|#
