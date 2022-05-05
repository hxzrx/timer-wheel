(in-package #:timer-wheel)

(log:debug :debug)

(defconstant +milliseconds-per-second+ 1000)
(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

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
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (sec nsec) (local-time::%get-current-time)
    (the fixnum (+ (the fixnum (* (+ (the fixnum sec) +unix-epoch+)
                                  +milliseconds-per-second+))
                   (truncate (/ (the fixnum nsec) 1000000))))))

#+:ignore
(defun get-current-universal-milliseconds () ; slow
  "Get the universal time, in millisecond"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((now (local-time:now))
         (msec (the fixnum (/ (the fixnum (local-time:nsec-of now)) 1000))))
    (the fixnum (+ (the fixnum (* (the fixnum (local-time:timestamp-to-universal now))
                                  +milliseconds-per-second+))
                   msec))))

(declaim (inline get-current-universal-milliseconds))
(defun current-universal-milliseconds ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (get-current-universal-milliseconds))

(declaim (inline get-local-timezone))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-local-timezone ()
    "Get time zone number of the local machine,
plus number for the eastern time zones and minus number for the western time zones.
So, the time zone of Beijing is 8."
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (- (the fixnum (nth-value 8 (decode-universal-time (get-universal-time)))))))

(defconstant +local-timezone+ (get-local-timezone))

(declaim (inline timestring->timestamp))
(defun timestring->timestamp (timestring)
  "Parse a timestring to a timestamp object with the time zone converted to the local zone.
The timestring should have at least two parts: date and time, concatenated with #\T or #\Space.
See the examples below."
  ;; (timestring->timestamp "2022-03-24 16:28:00") ; default to local zone
  ;; (timestring->timestamp "2022-03-24T16:28:00")
  ;; (timestring->timestamp "2022-03-24T16:28:00+08:00")
  ;; (timestring->timestamp "2022-03-24T16:28:00.123456+08:00")
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (local-time:parse-timestring timestring
                               :fail-on-error t
                               :date-time-separator (cond ((char= (schar timestring 10) #\T) #\T)
                                                          ((char= (schar timestring 10) #\Space) #\Space)
                                                          ((char= (schar timestring 10) #\t) #\t)
                                                          (t (error "Invalid timestring format: ~d" timestring)))
                               :allow-missing-date-part nil
                               :allow-missing-time-part nil
                               :allow-missing-timezone-part t
                               :offset (* +local-timezone+ 60 60)))

(declaim (inline timestamp->timestring))
(defun timestamp->timestring (timestamp &key (format *default-time-format*) (timezone local-time:*default-timezone*))
  "Convert a timestamp object to a timestring, default iso-3339 format and local time zone."
  ;; (timestamp-timestring (local-time:now))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (local-time:timestamp timestamp))
  (local-time:format-timestring nil timestamp :format format :timezone timezone))

(declaim (inline timestamp->universal-milliseconds))
(defun timestamp->universal-milliseconds (timestamp)
  "Convert a timestamp object to universal time, in milliseconds"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (local-time:timestamp timestamp))
  (the fixnum (+ (the fixnum (* (the fixnum (local-time:timestamp-to-universal timestamp)) +milliseconds-per-second+))
                 (the fixnum (local-time:timestamp-millisecond timestamp)))))

(declaim (inline timestring->universal-milliseconds))
(defun timestring->universal-milliseconds (timestring)
  "Convert a timestring to universal time, in milliseconds"
  ;; (timestring->universal-milliseconds (timestamp->timestring (local-time:now)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (string timestring))
  (let ((timestamp (the local-time:timestamp (timestring->timestamp timestring))))
    (the fixnum (+ (the fixnum (* (the fixnum (local-time:timestamp-to-universal timestamp)) +milliseconds-per-second+))
                   (the fixnum (local-time:timestamp-millisecond timestamp))))))

(declaim (inline universal-milliseconds->timestamp))
(defun universal-milliseconds->timestamp (universal)
  "Convert a universal time (in milliseconds) to a timestamp object."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (sec msec) (floor (the fixnum universal) 1000)
    (local-time:universal-to-timestamp sec :nsec (* msec 1000000))))

(defmacro when-let (bindings &body forms) ; alexandria's when-let
  "BINDINGS must be either single binding of the form:
 (variable initial-form)
or a list of bindings of the form:
 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))
"
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro if-let (bindings &body (then-form &optional else-form)) ; alexandria's if-let
  "BINDINGS must be either single binding of the form:
 (variable initial-form)
or a list of bindings of the form:
 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))
"
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))


;;; fifo queue apis for safe accessing
;;; completely copied from cl-trivial-pool

(defun make-queue (&optional (init-length 100) (unbound t) (name (string (gensym "QUEUE-"))))
  "Return an unbound"
  (declare (ignore unbound))
  #-sbcl(declare (ignore name))
  #+sbcl(declare (ignore init-length))
  #+sbcl(sb-concurrency:make-queue :name name)
  #-sbcl(cl-fast-queues:make-safe-fifo :init-length init-length))

(defun enqueue (item queue)
  #+sbcl(sb-concurrency:enqueue item queue)
  #-sbcl(cl-fast-queues:enqueue item queue))

(defun dequeue (queue)
  #+sbcl(sb-concurrency:dequeue queue)
  #-sbcl(alexandria:when-let (val (cl-fast-queues:dequeue queue))
          (if (eq val cl-fast-queues:*underflow-flag*)
              nil
              val)))

(defun queue-count (queue)
  #+sbcl(sb-concurrency:queue-count queue)
  #-sbcl(cl-fast-queues:queue-count queue))

(defun queue-to-list (queue)
  #+sbcl(sb-concurrency:list-queue-contents queue)
  #-sbcl(cl-fast-queues:queue-to-list queue))

(defun flush-queue (queue)
  "Flush the queue to an empty queue. The returned value should be neglected."
  (declare (optimize speed))
  #+sbcl
  (loop (let* ((head (sb-concurrency::queue-head queue))
               (tail (sb-concurrency::queue-tail queue))
               (next (cdr head)))
          (typecase next
            (null (return nil))
            (cons (when (and (eq head (sb-ext:compare-and-swap (sb-concurrency::queue-head queue)
                                                               head head))
                             (eq nil (sb-ext:compare-and-swap (cdr (sb-concurrency::queue-tail queue))
                                                              nil nil)))
                    (setf (car tail) sb-concurrency::+dummy+
                          (sb-concurrency::queue-head queue) (sb-concurrency::queue-tail queue))
                    (return t))))))
  #-sbcl(cl-fast-queues:queue-flush queue))

(defun queue-empty-p (queue)
  #+sbcl(sb-concurrency:queue-empty-p queue)
  #-sbcl(cl-fast-queues:queue-empty-p queue))


;;; atomic operations
;;; almost completely copied from cl-trivial-pool

(defun make-atomic (init-value)
  "Return a structure that can be cas'ed"
  #+ccl
  (make-array 1 :initial-element init-value)
  #-ccl
  (cons init-value nil))

(defmacro atomic-place (atomic-structure)
  "Return value of atomic-fixnum in macro."
  #+ccl
  `(svref ,atomic-structure 0)
  #-ccl
  `(car ,atomic-structure))

(defmacro atomic-incf (place &optional (diff 1))
  "Atomic incf fixnum in `place' with `diff' and return OLD value."
  #+sbcl
  `(sb-ext:atomic-incf ,place ,diff)
  #+ccl
  `(let ((old ,place))
     (ccl::atomic-incf-decf ,place ,diff)
     old))

(defmacro atomic-decf (place &optional (diff 1))
  "Atomic decf fixnum in `place' with `diff' and return OLD value."
  #+sbcl
  `(sb-ext:atomic-decf ,place ,diff)
  #+ccl
  `(let ((old ,place))
     (ccl::atomic-incf-decf ,place (- ,diff))
     old))

(defmacro compare-and-swap (place old new)
  "Atomically stores NEW in `place' if `old' value matches the current value of `place'.
Two values are considered to match if they are EQ.
return T if swap success, otherwise return NIL."
  ;; https://github.com/Shinmera/atomics/blob/master/atomics.lisp
  #+sbcl
  (let ((tmp (gensym "OLD")))
    `(let ((,tmp ,old)) (eq ,tmp (sb-ext:cas ,place ,tmp ,new))))
  #+ccl
  `(ccl::conditional-store ,place ,old ,new)
  #+clasp
  (let ((tmp (gensym "OLD")))
    `(let ((,tmp ,old)) (eq ,tmp (mp:cas ,place ,tmp ,new))))
  #+ecl
  (let ((tmp (gensym "OLD")))
    `(let ((,tmp ,old)) (eq ,tmp (mp:compare-and-swap ,place ,tmp ,new))))
  #+allegro
  `(if (excl:atomic-conditional-setf ,place ,new ,old) T NIL)
  #+lispworks
  `(system:compare-and-swap ,place ,old ,new)
  #+mezzano
  (let ((tmp (gensym "OLD")))
    `(let ((,tmp ,old))
       (eq ,tmp (mezzano.extensions:compare-and-swap ,place ,tmp ,new))))
  #-(or allegro ccl clasp ecl lispworks mezzano sbcl)
  (no-support 'CAS))

(defmacro atomic-update (place function &rest args)
  "Atomically swap value in `place' with `function' called and return new value."
  #+sbcl
  `(sb-ext:atomic-update ,place ,function ,@args)
  #-sbcl
  (alexandria:with-gensyms (func old-value new-value)
    `(loop :with ,func = ,function
           :for ,old-value = ,place
           :for ,new-value = (funcall ,func ,old-value ,@args)
           :until (compare-and-swap ,place ,old-value ,new-value)
           :finally (return ,new-value))))

(defmacro atomic-set (place new-value)
  "Atomically update the `place' with `new-value'"
  ;; (atomic-set (atomic-place (make-atomic 0)) 100)
  `(atomic-update ,place
                  #'(lambda (x)
                      (declare (ignore x))
                      ,new-value)))

(defmacro atomic-peek (place)
  "Atomically get the value of `place' without change it."
  #+sbcl
  `(progn
     (sb-thread:barrier (:read))
     ,place)
  #-sbcl
  (alexandria:with-gensyms (val)
    `(loop for ,val = ,place
           until (compare-and-swap ,place ,val ,val)
           finally (return ,val))))

(defun peek-queue (queue)
  (declare (optimize speed))
  "Return the first item to be dequeued without dequeueing it"
  #-sbcl(cl-fast-queues:queue-peek queue)
  #+sbcl
  (loop (let* ((head (sb-concurrency::queue-head queue))
               (next (cdr head)))
          (typecase next
            (null (return nil))
            (cons (when (compare-and-swap (sb-concurrency::queue-head queue)
                                          head head)
                    (return (car next))))))))

(defmacro atomic-exchange (place new-value &environment env)
  "Atomic set value in `place' to `new-value' and return OLD value."
  #+sbcl
  (multiple-value-bind (vars vals old new cas-form read-form)
      (sb-ext:get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = ,new-value
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,old))))
  #-sbcl
  (declare (ignore env))
  #-sbcl
  (let ((old-symbol (gensym "old")))
    `(loop
       for ,old-symbol fixnum = ,place
       while (not (compare-and-swap ,place ,old-symbol ,new-value))
       finally (return ,old-symbol))))
