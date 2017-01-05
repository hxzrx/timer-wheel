;;;; timer-wheel-examples.asd

(asdf:defsystem #:timer-wheel.examples
  :description "Examples for the timer wheel library."
  :author "Nick Patrick <npatrick04@gmail.com"
  :license "MIT"
  :depends-on (#:bordeaux-threads #:timer-wheel)
  :serial t
  :components ((:module "examples"
			:components
			((:file "package")
			 (:file "timer-test")
			 (:file "simple")
			 (:file "execute-timeout")))))

