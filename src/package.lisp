;;;; package.lisp

(defpackage #:timer-wheel
  (:use #:cl)
  (:nicknames #:tw)
  (:export #:*wheel-size*
	   #:*resolution*
	   
	   #:timer
	   #:wheel

	   #:make-timer
	   #:make-wheel

	   ;; A condition, and the symbol name for a timer that's not scheduled
	   #:unscheduled

	   #:schedule-timer
	   #:uninstall-timer
	   #:remaining

	   #:initialize-timer-wheel
	   #:shutdown-timer-wheel
	   #:with-timer-wheel))

