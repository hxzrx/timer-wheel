;;;; timer-wheel.asd

(asdf:defsystem #:timer-wheel
  :description "Describe timer-wheel here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:bordeaux-threads #:rt-events)
  :serial t
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "utils")
			 (:file "bt-timeout")
			 (:file "timer-wheel")))))

