(in-package :timer-wheel-tests)

(log:debug :debug)

(define-test positive :parent utils
  (of-type timer-wheel::positive-real 1)
  (of-type timer-wheel::positive-fixnum 1)
  (false (typep 0 'timer-wheel::positive-real))
  (false (typep 0 'timer-wheel::positive-fixnum)))

(define-test current-universal-milliseconds :parent utils
  (let ((now0 (tw::get-current-universal-milliseconds))
        (now1 (tw::current-universal-milliseconds))
        (now2 (tw::timestamp->universal-milliseconds (local-time:now))))
    (log:debug "now0: ~d, now1: ~d, now2: ~d" now0 now1 now2)
    (true (<= (- now2 now0) 2)) ; even (= (- now2 now0) 0)) will pass most of the time
    (true (<= (- now2 now1) 2))))

#+gmt+8
(define-test get-local-timezone :parent utils
  (is = 8 (tw::get-local-timezone)))

(define-test timestring->timestamp :parent utils
  (let ((ts1 "2022-03-24 16:28:00")
        (ts2 "2022-03-24T16:28:00")
        (ts3 "2022-03-24T16:28:00+08:00")
        (ts4 "2022-03-24T16:28:00.123456+08:00"))
    (finish (tw::timestring->timestamp ts1))
    (finish (tw::timestring->timestamp ts2))
    (finish (tw::timestring->timestamp ts3))
    (finish (tw::timestring->timestamp ts4))
    (log:debug "timestring: ~d, timestamp: ~d" ts1 (tw::timestring->timestamp ts1))
    (log:debug "timestring: ~d, timestamp: ~d" ts2 (tw::timestring->timestamp ts2))
    (log:debug "timestring: ~d, timestamp: ~d" ts3 (tw::timestring->timestamp ts3))
    (log:debug "timestring: ~d, timestamp: ~d" ts4 (tw::timestring->timestamp ts4))))

(define-test timestamp->timestring :parent utils
  (let ((now (local-time:now)))
    (finish (tw::timestamp->timestring now))
    (log:debug "timestamp: ~d, timestring: ~d" now (tw::timestamp->timestring now))))

(define-test timestamp->universal-milliseconds :parent utils
  (let ((now1 (local-time:now))
        (now2 (tw::get-current-universal-milliseconds)))
    (finish (tw::timestamp->universal-milliseconds now1))
    (true (<= (- now2 (tw::timestamp->universal-milliseconds now1)) 2))))

(define-test timestring->universal-milliseconds-1 :parent utils
  (let* ((now-ms (tw::get-current-universal-milliseconds))
         (timestamp (tw::universal-milliseconds->timestamp now-ms))
         (timestring (tw::timestamp->timestring timestamp)))
    (is = now-ms (tw::timestring->universal-milliseconds timestring))))

(define-test timestring->universal-milliseconds-2 :parent utils
  ;; will pass only in GMT+8
  (let ((ts1 "2022-03-24 16:28:00")
        (ts2 "2022-03-24T16:28:00")
        (ts3 "2022-03-24T16:28:00.000")
        (ts4 "2022-03-24T16:28:00.000000")
        (ts5 "2022-03-24T16:28:00+08:00")
        (ts6 "2022-03-24T16:28:00+8:00")
        (ts7 "2022-03-24T16:28:00.000+08:00")
        (ts8 "2022-03-24T16:28:00.000000+08:00"))
    (is = (tw::timestring->universal-milliseconds ts1) (tw::timestring->universal-milliseconds ts2))
    #+gmt+8(is = (tw::timestring->universal-milliseconds ts1) (tw::timestring->universal-milliseconds ts3))
    #+gmt+8(is = (tw::timestring->universal-milliseconds ts1) (tw::timestring->universal-milliseconds ts4))
    #+gmt+8(is = (tw::timestring->universal-milliseconds ts1) (tw::timestring->universal-milliseconds ts5))
    #+gmt+8(is = (tw::timestring->universal-milliseconds ts1) (tw::timestring->universal-milliseconds ts6))
    #+gmt+8(is = (tw::timestring->universal-milliseconds ts1) (tw::timestring->universal-milliseconds ts7))
    #+gmt+8(is = (tw::timestring->universal-milliseconds ts1) (tw::timestring->universal-milliseconds ts8))))


(define-test timestamp->universal-milliseconds :parent utils
  (let* ((timestamp (local-time:now))
         (now-ts (tw::get-current-universal-milliseconds)))
    (true (<= (- now-ts (tw::timestamp->universal-milliseconds timestamp)) 2))))
