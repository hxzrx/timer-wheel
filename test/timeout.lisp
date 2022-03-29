(in-package :timer-wheel-tests)

(define-test inspect-bt-timeout-context :parent timeout
  (let ((context (tw::make-bt-context)))
    (log:info "context: ~d" context)
    (finish (tw::inspect-bt-timeout-context context))))
