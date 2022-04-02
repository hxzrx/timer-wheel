;;;; timer-wheel-examples.asd

(asdf:defsystem #:timer-wheel.examples
  :description "Examples for the timer wheel library."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (#:bordeaux-threads #:timer-wheel)
  :serial t
  :components ((:module "examples"
		:components
		((:file "package")
		 (:file "timer-test")
		 (:file "simple")
		 (:file "execute-timeout")
                 (:file "perform")
                 (:file "verify-threads")))))
