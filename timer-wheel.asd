;;;; timer-wheel.asd

(asdf:defsystem #:timer-wheel
  :description "A timer wheel implementation with BORDEAUX-THREADS backend.
This is a fork of Nick Patrick's timer-wheel <https://github.com/npatrick04/timer-wheel> with several aspects got enhanced."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :version 1.0.0
  :depends-on (#:bordeaux-threads
               #:local-time
               #:cl-fast-queues
               #:log4cl)
  :serial t
  :in-order-to ((test-op (test-op "timer-wheel.test")))
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "utils")
			 (:file "bt-timeout")
			 (:file "timer-wheel")))))
