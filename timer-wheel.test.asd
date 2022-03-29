(asdf:defsystem #:timer-wheel.test
  :description "Tests for timer-wheel."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (#:timer-wheel #:parachute #:log4cl)
  :serial t
  :components ((:module "test"
			:components
			((:file "package")
			 (:file "test"))))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :timer-wheel-tests)))
