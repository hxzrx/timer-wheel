;;;; In this file, two very simple timers were created to test the speed of the wheel.
;;;; The times were made beforehand and the callbacks almost did nothing and thus the tests reflected the scheduling rate.
;;;; The (very) not strict results showed that this library can schedule at the rate of
;;;;   about 100M times per second for the timers without delay, and
;;;;   about  60M times per second for the timers with delay.
;;;; My test env: openSUSE Leap 15.3 hosted on win10 with vmware, the vm has 6 cores and 16G memory, sbcl 2.2.3.

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

(defun simple-schedule-perform-delay (wheel timer n)
  (defparameter *counter* (list 0))
  (time (dotimes (i (expt 10 n))
          (schedule-timer wheel timer 0.22))))


;; --- without delay ---

#|
(simple-schedule-perform *perform-wheel-1* *perform-timer-simple-1* 9)
Evaluation took:
  8.756 seconds of real time
  8.856266 seconds of total run time (8.856266 user, 0.000000 system)
  101.14% CPU
  22,700,966,628 processor cycles
  32,588,224 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-1* *perform-timer-counter-1* 9)
Evaluation took:
  8.764 seconds of real time
  8.851671 seconds of total run time (8.851671 user, 0.000000 system)
  101.00% CPU
  22,719,150,210 processor cycles
  15,905,040 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-2* *perform-timer-simple-2* 9)
Evaluation took:
  8.852 seconds of real time
  8.890066 seconds of total run time (8.890066 user, 0.000000 system)
  100.43% CPU
  22,947,602,787 processor cycles
  15,014,992 bytes consed
|#

#|
(simple-schedule-perform *perform-wheel-2* *perform-timer-counter-2* 9)
Evaluation took:
  8.804 seconds of real time
  8.828291 seconds of total run time (8.828291 user, 0.000000 system)
  100.27% CPU
  22,819,677,915 processor cycles
  3,206,336 bytes consed
|#

;; --- with delay ---

#|
(simple-schedule-perform-delay *perform-wheel-1* *perform-timer-simple-1* 9)
Evaluation took:
  15.652 seconds of real time
  15.692802 seconds of total run time (15.692802 user, 0.000000 system)
  100.26% CPU
  40,570,310,150 processor cycles
  117,296 bytes consed
|#

#|
(simple-schedule-perform-delay *perform-wheel-1* *perform-timer-counter-1* 9)
Evaluation took:
  15.836 seconds of real time
  15.866518 seconds of total run time (15.866518 user, 0.000000 system)
  100.20% CPU
  41,050,763,654 processor cycles
  211,328 bytes consed
|#

#|
(simple-schedule-perform-delay *perform-wheel-2* *perform-timer-simple-2* 9)
Evaluation took:
  15.968 seconds of real time
  15.995108 seconds of total run time (15.995108 user, 0.000000 system)
  100.17% CPU
  41,395,570,363 processor cycles
  150,048 bytes consed
|#

#|
(simple-schedule-perform-delay *perform-wheel-2* *perform-timer-counter-2* 9)
  Evaluation took:
  15.988 seconds of real time
  16.015091 seconds of total run time (16.015091 user, 0.000000 system)
  100.17% CPU
  41,451,357,941 processor cycles
  172,288 bytes consed
|#
