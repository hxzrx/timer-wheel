(in-package #:timer-wheel)

(defconstant +milliseconds-per-second+ 1000)
(defclass timeout-context ()
  ((resolution :accessor context-resolution
	       :initarg :resolution)
   (timeout-overrun :accessor timeout-overrun
		    :initform 0)))
