;;;; package.lisp

(defpackage #:timer-wheel
  (:use #:cl)
  (:nicknames #:tw)
  (:export #:*default-size*
	   #:*default-resolution*
	   
	   #:timer
	   #:wheel

	   #:make-timer
	   #:make-wheel

	   ;; A condition, and the symbol name for a timer that's not scheduled
	   #:unscheduled

	   #:schedule-timer
	   #:uninstall-timer
	   #:remaining
	   #:wheel-resolution

	   #:initialize-timer-wheel
	   #:shutdown-timer-wheel
	   #:with-timer-wheel))

