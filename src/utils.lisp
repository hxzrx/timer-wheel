(in-package #:timer-wheel)

(defconstant +milliseconds-per-second+ 1000)

(log:debug :debug)

(deftype positive-real ()
  '(real (0) *))

(deftype positive-fixnum ()
  `(integer 1 ,most-positive-fixnum))

(defparameter *default-time-format*
  '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\. (:USEC 6) :GMT-OFFSET-OR-Z)
  "rfc3339 format")

(defclass timeout-context ()
  ((resolution :accessor context-resolution :initarg :resolution :initform nil)
   (timeout-overrun :accessor timeout-overrun :initform 0)))


;; utils about time

(declaim (inline current-universal-milliseconds))
(defun get-current-universal-milliseconds ()
  "Get the universal time, in millisecond"
  (multiple-value-bind (sec nsec) (local-time::%get-current-time)
    (+ (* (+ sec (encode-universal-time 0 0 0 1 1 1970 0))
          +milliseconds-per-second+)
       (truncate (/ nsec 1000000)))))

(declaim (inline get-current-universal-milliseconds))
(defun current-universal-milliseconds ()
  (get-current-universal-milliseconds))

(defun get-local-timezone ()
  "Get time zone number of the local machine,
plus number for the eastern time zones and minus number for the western time zones.
So, the time zone of Beijing is 8."
  (- (nth-value 8 (decode-universal-time (get-universal-time)))))

(defun timestring->timestamp (timestring)
  "Parse a timestring to a timestamp object with the time zone converted to the local zone.
The timestring should have at least two parts: date and time, concatenated with #\T or #\Space.
See the examples below."
  ;; (timestring->timestamp "2022-03-24 16:28:00") ; default to local zone
  ;; (timestring->timestamp "2022-03-24T16:28:00")
  ;; (timestring->timestamp "2022-03-24T16:28:00+08:00")
  ;; (timestring->timestamp "2022-03-24T16:28:00.123456+08:00")
  (declare (string timestring))
  (local-time:parse-timestring timestring
                               :fail-on-error t
                               :date-time-separator (cond ((char= (aref timestring 10) #\T) #\T)
                                                          ((char= (aref timestring 10) #\Space) #\Space)
                                                          ((char= (aref timestring 10) #\t) #\t)
                                                          (t (error "Invalid timestring format: ~d" timestring)))
                               :allow-missing-date-part nil
                               :allow-missing-time-part nil
                               :allow-missing-timezone-part t
                               :offset (* (get-local-timezone) 60 60)))

(defun timestamp->timestring (timestamp &key (format *default-time-format*) (timezone local-time:*default-timezone*))
  "Convert a timestamp object to a timestring, default iso-3339 format and local time zone."
  ;; (timestamp-timestring (local-time:now))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (local-time:timestamp timestamp))
  (local-time:format-timestring nil timestamp :format format :timezone timezone))

(defun timestamp->universal-milliseconds (timestamp)
  "Convert a timestamp object to universal time, in milliseconds"
  (declare (local-time:timestamp timestamp))
  (+ (* (local-time:timestamp-to-universal timestamp) +milliseconds-per-second+)
     (local-time:timestamp-millisecond timestamp)))

(defun timestring->universal-milliseconds (timestring)
  "Convert a timestring to universal time, in milliseconds"
  ;; (timestring->universal-milliseconds (timestamp->timestring (local-time:now)))
  (declare (string timestring))
  (let ((timestamp (timestring->timestamp timestring)))
    (+ (* (local-time:timestamp-to-universal timestamp) +milliseconds-per-second+)
       (local-time:timestamp-millisecond timestamp))))

(defun universal-milliseconds->timestamp (universal)
  "Convert a universal time (in milliseconds) to a timestamp object."
  (multiple-value-bind (sec msec) (floor universal 1000)
    (local-time:universal-to-timestamp sec :nsec (* msec 1000000))))
