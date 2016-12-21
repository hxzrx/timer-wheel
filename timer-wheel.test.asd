(asdf:defsystem #:timer-wheel.test
  :description "Tests for timer-wheel."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :depends-on (#:bordeaux-threads #:fiveam)
  :serial t
  :components ((:module "test"
			:components
			((:file "package")
			 (:file "test")))))
