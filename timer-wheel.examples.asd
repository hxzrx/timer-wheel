;;;; timer-wheel-examples.asd

(asdf:defsystem #:timer-wheel.examples
  :description "Describe timer-wheel here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:bordeaux-threads #:timer-wheel)
  :serial t
  :components ((:module "examples"
			:components
			((:file "package")
			 (:file "timer-test")
			 (:file "simple")
			 (:file "execute-timeout")))))

