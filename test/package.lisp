(defpackage #:timer-wheel-tests
  (:use #:cl #:timer-wheel #:parachute)
  (:export #:test
           #:timer-wheel-tests))

(in-package :timer-wheel-tests)


(define-test timer-wheel-tests)

(define-test utils       :parent timer-wheel-tests)
(define-test timeout     :parent timer-wheel-tests)
(define-test timer-wheel :parent timer-wheel-tests)
