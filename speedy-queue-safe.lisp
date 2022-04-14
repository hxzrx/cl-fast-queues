;;;; This file was initially copied from speedy-queue.lisp
;;;; In this safe version, for the sake of optimization,
;;;; a fixnum takes place of the first of the array,
;;;; with the higher half bits show the pop place
;;;; and the lower half bits show the push place.

(cl:defpackage #:cl-speedy-queue-safe
  (:use :cl)
  (:export
   :make-queue
   :queue-to-list
   :list-to-queue
   :queue-count
   :queue-length
   :queue-peek
   :queue-full-p
   :queue-empty-p
   :enqueue
   :dequeue
   :queue-find
   :queue-flush
   :*overflow-flag*
   :*underflow-flag*))

(cl:in-package #:cl-speedy-queue-safe)

(defvar *overflow-flag* cl-speedy-lifo:*overflow-flag*)
;;:overflow-A6AC128A-4385-4C54-B384-8D687456C10A)

(defvar *underflow-flag* cl-speedy-lifo:*underflow-flag*)
;;:underflow-80B88679-7DD0-499E-BAE9-673167980515)

;;; The functions in this file are dangerous. Good compilers will generate code that will
;;;   do VERY funky shit when called incorrectly. Calls to these functions should be hidden
;;;   behind safe code that never passes arguments of incorrect types.

;;; Unlike the standard queue implementation which you find in CL code (push to the tail of
;;;   a list, pop from the head), these queues do not cons one bit. You do, however, need to
;;;   "declare" a queue's size (at runtime) when you make one.

(defmacro define-speedy-function (name args &body body)
  `(progn (declaim (inline ,name))
          (defun ,name ,args
            (declare (optimize (speed 3) (safety 0) (debug 0)))
            ,@body)))

;;; Queue Condition API

(define-condition queue-condition (error)
  ((queue :reader queue-condition-queue :initarg :queue))
  (:report (lambda (c s)
             (format s "Queue error in queue ~S"
                     (queue-condition-queue c)))))
(define-condition queue-length-error (queue-condition)
  ((attempted-length :reader queue-error-attempted-length :initarg :attempted-length))
  (:report (lambda (c s)
             (format s "Queue created with invalid length: ~S"
                     (queue-error-attempted-length c)))))
(define-condition queue-overflow-error (queue-condition)
  ((item :reader queue-overflow-extra-item :initarg :item))
  (:report (lambda (c s)
             (format s "Queue ~S is full, and can't have ~S stuffed into it"
                     (queue-condition-queue c) (queue-overflow-extra-item c)))))
(define-condition queue-underflow-error (queue-condition) ()
  (:report (lambda (c s)
             (format s "Queue ~S is empty, and can't be dequeued anymore"
                     (queue-condition-queue c)))))

(eval-when (:compile-toplevel)
  (defvar queue-sentinel (make-symbol "EMPTY"))
  (defvar +fixnum-bits+ (floor (log most-positive-fixnum 2)))  ; 62 for sbcl x86-64, 60 for ccl x86-64
  (defvar +queue-length-bits+ (floor (/ +fixnum-bits+ 2))) ; 31
  (defvar +low-bits-ones+  (1- (expt 2 +queue-length-bits+))) ; 2147483647, 31 bits of ones
  (defvar +high-bits-ones+ (ash +low-bits-ones+ +queue-length-bits+)) ; 31 higher bits of ones, 31 lower bits of zeros
  (defvar +max-queue-length+ +low-bits-ones+)) ; 2147483647


(define-speedy-function %make-queue (length)
  "Creates a new queue of maximum size LENGTH"
  (when (typep length 'fixnum)
    (locally (declare (fixnum length))
      (when (<= 1 length #.+max-queue-length+)
        (let ((queue (make-array (the fixnum (+ 1 length)) :initial-element nil)))
          (setf (svref queue 0) #.(+ (ash 1 +queue-length-bits+) 1)
                (svref queue 1) '#.queue-sentinel)
          (return-from %make-queue queue)))))
  (error 'queue-length-error :attempted-length length))

;;; Do we need a compiler macro for the above when LENGTH is constant so that we
;;;   don't add 2 at runtime? That's not very high on the priority list, although
;;;   it'll probably take less time to write than this comment did. -- Adlai

(define-speedy-function %queue-length (queue)
  "Returns QUEUE's maximum length"
  (the fixnum (- (length (the simple-vector queue)) 1)))

(define-speedy-function queuep (x)
  "If this returns NIL, X is not a queue"
  (when (simple-vector-p x)
    (let* ((flag   (the fixnum (svref x 0)))
           (length (the fixnum (length x)))
           (head   (the fixnum (ash flag #.(- +queue-length-bits+)))) ; -31
           (tail   (the fixnum (logand flag #.+low-bits-ones+))))
      (and (<= 1 head length)
           (<= 1 tail length)
           (<= length #.+low-bits-ones+)))))

(define-speedy-function %queue-out (queue)
  "QUEUE's exit pointer"
  ;; higher 32 bits
  ;;(the fixnum (ldb (byte 31 32) (the fixnum (svref queue 0))))
  (the fixnum (ash (the fixnum (svref queue 0)) #.(- +queue-length-bits+)))) ; -31

(define-speedy-function %queue-in (queue)
  "QUEUE's entry pointer"
  ;; lower 32 bits
  ;;(the fixnum (ldb (byte 31 0) (the fixnum (svref queue 1))))
  (the fixnum (logand (the fixnum (svref queue 0)) #.+low-bits-ones+)))

(define-speedy-function %queue-peek (queue)
  "Dereference QUEUE's exit pointer"
  (svref queue (%queue-out queue)))

(define-speedy-function %queue-zero-p (queue)
  "Checks whether QUEUE's theoretical length is zero"
  (= (the fixnum (%queue-in queue))
     (the fixnum (%queue-out queue))))

(define-speedy-function %queue-empty-p (queue)
  "Checks whether QUEUE is effectively empty"
  (eq (svref queue (%queue-out queue)) '#.queue-sentinel))

(define-speedy-function %queue-full-p (queue)
  "Checks whether QUEUE is effectively full"
  ;; We keep the exit reference around because we do two checks
  (let ((out (%queue-out queue)))
    (declare (fixnum out))
    ;; Are the entry and exit pointers the same?
    (when (= out (the fixnum (%queue-in queue)))
      ;; Is there a real value at the exit pointer?
      (not (eq (svref queue out) '#.queue-sentinel)))))

(define-speedy-function %queue-count (queue)
  "Returns QUEUE's effective length"
  ;; We start with the 'raw' length -- the difference between the pointers
  (let ((length (- (%queue-in queue) (%queue-out queue))))
    (declare (fixnum length))
    (cond ((plusp length) length)                         ; Raw length is OK
          ((or (minusp length)                            ; Entry pointer is before exit pointer,
               (not (eq (%queue-peek queue)               ;   or the queue is full if the pointers
                        '#.queue-sentinel)))              ;   don't point to the sentinel value, so
           (the fixnum (+ length (%queue-length queue)))) ; Add the effective length
          (t 0))))                                        ; Queue is empty -- return zero

(define-speedy-function %next-index (current-index queue-real-length)
  (declare (fixnum current-index queue-real-length))
  (let ((new-index (1+ current-index)))                 ; Simply increment the index
    (declare (fixnum new-index))
    (the fixnum (if (= new-index queue-real-length) 1 new-index))))  ; Overflow to 1 if necessary

(define-speedy-function %enqueue (object queue)
  ;;(declare (fixnum in))
  "Enqueue OBJECT and increment QUEUE's entry pointer"
  (let* ((flag (the fixnum (svref queue 0)))
         (out (ash flag #.(- +queue-length-bits+)))
         (in (the fixnum (logand flag #.+low-bits-ones+))))
    (if (or (not (= in out))
            (eq (svref queue in) '#.queue-sentinel))
        (prog1 (setf (svref queue in) object)
          (setf (svref queue 0) (the fixnum (+ (logand flag #.+high-bits-ones+)
                                               (%next-index in (length (the (simple-vector *) queue)))))))
        *overflow-flag*
        )))

(define-speedy-function %dequeue (queue keep-in-queue-p)
  "Sets QUEUE's tail to QUEUE, increments QUEUE's tail pointer, and returns the previous tail ref"
  (let* ((flag (the fixnum (svref queue 0)))
         (out (ash flag #.(- +queue-length-bits+)))
         (in  (the fixnum (logand flag #.+low-bits-ones+)))
         (out-object (svref queue out)))
    (if (eq out-object '#.queue-sentinel)
        *underflow-flag*
        (prog1 out-object
          (unless keep-in-queue-p (setf (svref queue out) nil))
          (setf (svref queue 0)
                (the fixnum (+ (ash (if (= (incf out) (length (the (simple-vector *) queue)))
                                        (setf out 1) out)
                                    #.+queue-length-bits+)
                               in)))
          (when (= in out)
            (setf (svref queue out) '#.queue-sentinel))))))

(define-speedy-function %queue-flush (queue)
  (setf (svref queue 0) #.(+ (ash 1 +queue-length-bits+) 1)
        (svref queue 1) '#.queue-sentinel)
  queue)

;;; Now that all the backend functions are defined, we can define the API:

(defun make-queue (size)
  "Makes a queue of maximum size SIZE"
  (declare (fixnum size))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%make-queue size))

(defun queue-count (queue)
  "Returns the current size of QUEUE"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-count queue))

(defun queue-length (queue)
  "Returns the maximum size of QUEUE"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-length queue))

(defun queue-peek (queue)
  "Returns the next item that would be dequeued without dequeueing it."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((peek (%queue-peek queue)))
    (if (eq peek '#.queue-sentinel)
        (values nil nil)
        (values peek t))))

(defun queue-full-p (queue)
  "Returns NIL if more items can be enqueued."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-full-p queue))

(defun queue-empty-p (queue)
  "Tests whether QUEUE is empty"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%queue-empty-p queue))

(defun enqueue (object queue)
  "Enqueues OBJECT in QUEUE"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%enqueue object queue))

(defun dequeue (queue &optional (keep-in-queue-p t))
  "Dequeues QUEUE.
When `keep-in-queue-p' sets to nil, the dequeued val will no longer keep an ref in the queue,
this is useful when the queue holds very big objects."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%dequeue queue keep-in-queue-p))

(defun queue-to-list (queue)
  "Conver a queue to a list, the first of the returned list is the first item to be popped."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-array queue))
  (if (queue-empty-p queue)
      nil
      (let ((in (%queue-in queue))
            (out (%queue-out queue)))
        (if (> in out)
            (coerce (subseq queue out in) 'list)
            (nconc (coerce (subseq queue out) 'list)
                   (coerce (subseq queue 2 in) 'list))))))

(defun list-to-queue (list)
  "Conver a list to a queue, the first of the returned queue is the car or the list."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-queue len)))
    (dolist (item list)
      (enqueue item))
    queue))

(defun queue-find (item queue &key (key #'identity) (test #'eql))
  "Find `item' in `queue', return the item that has been found."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function key test))
  (if (queue-empty-p queue)
      nil
      (let ((in (%queue-in queue))
            (out (%queue-out queue)))
        (if (> in out)
            (find item  queue :start out :end in :key key :test test)
            (or (find item queue :start out :key key :test test)
                (find item queue :start 2 :end in :key key :test test))))))

(defun queue-flush (queue)
  "Make `queue' empty"
  (%queue-flush queue))


;; ------- thread safe version -------







#|
;; conclusion: DO NOT use setf ldb in ccl!

(defparameter *int* #.(+ (ash 1 +queue-length-bits+) 1))

(defun test-ldb-rd-high (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (ldb (byte 31 31) n))

(defun test-ash-rd-high (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (ash n -31))

(defun time-rd-high (&optional (num *int*) (times 10000000000))
  ;; sbcl: 50.5s/42.6s
  ;; ccl:  37.2s/37.2
  (format t "~&test-ldb-rd~%")
  (time (dotimes (i times)
          (test-ldb-rd-high num)))
  (format t "~&~%test-ash-rd~%")
  (time (dotimes (i times)
          (test-ash-rd-high num))))


(defun test-ldb-rd-low (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (ldb (byte 31 0) n))

(defun test-ash-rd-low (n) ; logand in fact
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (logand n #.+low-bits-ones+))

(defun time-rd-low (&optional (num *int*) (times 10000000000))
  ;; sbcl: 41.3s/40.4s
  ;; ccl: 45.7s/34.7s
  (format t "~&test-ldb-rd~%")
  (time (dotimes (i times)
          (test-ldb-rd-low num)))
  (format t "~&~%test-ash-rd~%")
  (time (dotimes (i times)
          (test-ash-rd-low num))))


(defun test-logand-wt-high (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (setf n (+ (+ (logand n #.+high-bits-ones+) 0)
             (logand n #.+low-bits-ones+)))
  n)

(defun test-ldb-wt-high (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (setf (ldb (byte 31 31) n) (+ (ldb (byte 31 31) n) 0))
  n)

(defun test-ash-wt-high (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (setf n (+ (ash (+ (ash n #.(- +queue-length-bits+)) 0) #.+queue-length-bits+)
             (logand n #.+low-bits-ones+)))
  n)

(defun time-wt-high (&optional (num *int*) (times 10000000000))
  ;; sbcl: 41.9s/41.8s/44.3s
  ;; ccl: 43.3s/1291.6s/86.4s
  (format t "~&test-logand-wt-high~%")
  (time (dotimes (i times)
          (test-logand-wt-high num)))
  (format t "~&test-ldb-wt-high~%")
  (time (dotimes (i times)
          (test-ldb-wt-high num)))
  (format t "~&~%test-ash-wt-high~%")
  (time (dotimes (i times)
          (test-ash-wt-high num))))


(defun test-logand-wt-low (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (setf n (+ (logand n #.+high-bits-ones+)
             (+ (logand n #.+low-bits-ones+) 0)))
  n)

(defun test-ldb-wt-low (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (setf (ldb (byte 31 0) n) (+ (ldb (byte 31 0) n) 0))
  n)

(defun test-ash-wt-low (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum n))
  (setf n (+ (ash (ash n #.(- +queue-length-bits+)) #.+queue-length-bits+)
             (+ (logand n #.+low-bits-ones+) 0)))
  n)

(defun time-wt-low (&optional (num *int*) (times 10000000000))
  ;; sbcl: 41.3s/41.6s/45.0s
  ;; ccl: 42.5s/1209.8s/74.5s
  (format t "~&test-logand-wt-low~%")
  (time (dotimes (i times)
          (test-logand-wt-high num)))
  (format t "~&test-ldb-wt-low~%")
  (time (dotimes (i times)
          (test-ldb-wt-high num)))
  (format t "~&~%test-ash-wt-low~%")
  (time (dotimes (i times)
          (test-ash-wt-high num))))
|#

#|
;; for 10^9 times, about 1~3 seconds slower than speedy-queue.

(defparameter *times* (loop for i from 3 to 9
                            collect (expt 10 i)))

(dolist (num *times*) ; 15.516s for 10^9
  (format t "FIFO queue, push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((queue (make-queue num)))
    (time (progn
            (dotimes (i num)
              (enqueue i queue))
            (dotimes (i num)
              (dequeue queue nil))))))

(dolist (num *times*) ; 17.988s for 10^9
  (format t "FIFO queue, push+pop, with consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((queue (make-queue num)))
          (dotimes (i num)
            (enqueue i queue))
          (dotimes (i num)
            (dequeue queue nil)))))

(dolist (num *times*) ; crashed for 10^9 in my machine
  (format t "LIST queue, push+pop: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((list-queue nil))
          (dotimes (i num)
            (cl:push i list-queue))
          (dotimes (i num)
            (cl:pop list-queue)))))
|#
