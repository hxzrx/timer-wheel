;;;; timer-wheel.asd

(asdf:defsystem #:timer-wheel
  :description "A timer wheel implementation with BORDEAUX-THREADS backend.
This is a fork of Nick Patrick's timer-wheel <https://github.com/npatrick04/timer-wheel> with several aspects got enhanced."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (#:bordeaux-threads
               #:local-time)
  :serial t
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "utils")
			 (:file "bt-timeout")
			 (:file "timer-wheel")))))
