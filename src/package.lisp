;;;; package.lisp

(defpackage #:timer-wheel
  (:use #:cl)
  (:nicknames #:tw)
  (:export #:*default-size*
	   #:*default-resolution*
	   #:*expired-epsilon*
           #:*wheel-list*

	   #:timer
	   #:wheel

	   #:make-wheel
           #:make-timer

	   ;; A condition, and the symbol name for a timer that's not scheduled
	   #:unscheduled

	   #:schedule-timer
           #:schedule-timer-simply
           #:attach-scheduler
	   #:uninstall-timer
	   #:remaining
	   #:wheel-resolution
           #:get-real-period

	   #:initialize-timer-wheel
	   #:shutdown-timer-wheel
	   #:with-timer-wheel
           #:with-timeout))
