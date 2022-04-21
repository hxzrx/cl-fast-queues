;;;; Test failed, DO NOT use it. modification after compare-and-swap is not atomic.

;; Design:
;; This safe queue keeps its enqueue place, dequeue place and full flag in a fixnum in the head of an simple-array.
;;
;; The 0th palce of an array is the flag of the queue, which is partitioned into:
;;      ﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍﹍
;;     |  dequeue bits  |  enqueue bits  |  unused bit  |  full bit   |
;;      ﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉﹉
;; bits: fixnumLen/2-1    fixnumLen/2-1         1             1
;;
;; full bit = 1 shows that the queue is full.
;;
;; Coding:
;; This queue tried to do setf if compare-and-swap return true, and here's the pseudo-code:
;;   (loop (let (...)
;;            (when (compare-and-swap place old new)
;;               (modify-something)
;;               (return))))
;;
;; Objective: We hope that the items returned by push threads will be those items enqueued.
;;
;; Result: FAIL
;;
;; Analyze:
;; The table below shows how this code FAILS.
;;   Suppose we have a queue, all push and pop operations are implemented with above codes
;;   and they run in seperate threads.
;; The test verifies this, with a very low probability.
;;
;; |---------------------------------------------------------------------|
;; | thread |      time1      |      time2          |      time3         |
;; |--------|-------|---------|-------|-------------|-------|------------|
;; |        | flag  |  queue  | flag  |    queue    | flag  |   queue    |
;; |--------|-------|---------|-------|-------------|-------|------------|
;; |  push  | 1/2/0 | nil nil |       |             | 2/2/0 |   1 nil    |
;; |  pop   |       |         | 2/2/0 |   nil nil   |       |            |
;; |--------|-------|---------|-------|-------------|-------|------------|
;; | return |       |         |       | pop ret nil |       | push ret 1 |
;; |---------------------------------------------------------------------|
;;
;; In this table, the "flag" columns show the status of enqueuePlace, dequeuePlace, isFull, for each time interval.
;;    and the "queue" columns only show the queue contents when some thread modify the contents of the queue,
;;    for each time interval.
;;
;; This example was took from a test, the result showed that the a thread pushed 1, but another thread popped NIL.


(ql:quickload :cl-speedy-lifo)
(ql:quickload :atomics)
(ql:quickload :bordeaux-threads)
(ql:quickload :parachute)

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bits-count* (integer-length most-negative-fixnum))  ; 62 for sbcl x86-64, 60 for ccl x86-64
  (defparameter *queue-length-bits* (1- (floor (/ *bits-count* 2)))) ; 30

  (defparameter *enqueue-start* 2)
  (defparameter *dequeue-start* (+ *enqueue-start* *queue-length-bits*)) ; 32 (from 0, so it's the 33nd place)

  (defparameter *enqueue-mask*  (ash (1- (expt 2 *queue-length-bits*)) *enqueue-start*)) ; 4294967292, 1111...11100, 30 ones
  (defparameter *dequeue-mask* (ash *enqueue-mask* *queue-length-bits*)) ; 4611686014132420608, 111...111000...000, 30 1s, 32 0s
  (defparameter *full-queue-mask* 1)

  (defparameter *max-queue-length* (1- (expt 2 *queue-length-bits*))) ; 1073741823

  (defparameter *overflow-flag* cl-speedy-lifo:*overflow-flag*)
  (defparameter *underflow-flag* cl-speedy-lifo:*underflow-flag*))

;;; The functions in this file are dangerous. Good compilers will generate code that will
;;;   do VERY funky shit when called incorrectly. Calls to these functions should be hidden
;;;   behind safe code that never passes arguments of incorrect types.

;;; Unlike the standard queue implementation which you find in CL code (push to the tail of
;;;   a list, pop from the head), these queues do not cons one bit. You do, however, need to
;;;   "declare" a queue's size (at runtime) when you make one.

(defmacro define-speedy-function (name args &body body)
  `(progn (declaim (inline ,name))
          (defun ,name ,args
            ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
            (declare (optimize (speed 0) (safety 3) (debug 3)))
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

(define-speedy-function %make-queue (length)
  "Creates a new queue of maximum size LENGTH, LENGTH should be at least two."
  (when (typep length 'fixnum)
    (locally (declare (fixnum length))
      (when (<= 2 length #.*max-queue-length*) ; this should move to the public make-queue
        (let ((queue (make-array (the fixnum (+ 1 length)) :initial-element nil)))
          (setf (svref queue 0) #.(+ (ash 1 *dequeue-start*) (ash 1 *enqueue-start*))) ; 4294967300, 1000...000100
          (return-from %make-queue queue)))))
  (error 'queue-length-error :attempted-length length))

;;; Do we need a compiler macro for the above when LENGTH is constant so that we
;;;   don't add 2 at runtime? That's not very high on the priority list, although
;;;   it'll probably take less time to write than this comment did. -- Adlai

(define-speedy-function %queue-length (queue)
  "Returns QUEUE's maximum length."
  (the fixnum (- (length (the simple-vector queue)) 1)))

(define-speedy-function queuep (x)
  "If this returns NIL, X is not a queue"
  (when (simple-vector-p x)
    (let* ((flag   (the fixnum (svref x 0)))
           (length (the fixnum (length x)))
           (head   (the fixnum (ash flag #.(- *dequeue-start*))))
           (tail   (the fixnum (ash (logand flag #.*enqueue-mask*)
                                    #.(- *enqueue-start*))))
           (fullp  (the fixnum (logand flag #.*full-queue-mask*))))
      (and (<= 1 head length)
           (<= 1 tail length)
           (<= length #.*max-queue-length*)
           (if fullp (= head tail) t)))))

(define-speedy-function %queue-out (queue)
  "QUEUE's exit pointer"
  (the fixnum (ash (logand (the fixnum (svref queue 0))
                           #.*dequeue-mask*)
                   #.(- *dequeue-start*))))

(define-speedy-function %queue-in (queue)
  "QUEUE's entry pointer"
  (the fixnum (ash (logand (the fixnum (svref queue 0))
                           #.*enqueue-mask*)
                   #.(- *enqueue-start*))))

(define-speedy-function %in=out (flag-num)
  "Test if a queue's first element shows that the enqueue place equal to dequeue place."
  (declare (fixnum flag-num))
  (= (the fixnum (ash (logand flag-num #.*enqueue-mask*) #.(- *enqueue-start*)))
     (the fixnum (ash (logand flag-num #.*dequeue-mask*) #.(- *dequeue-start*)))))

(define-speedy-function %out=in (flag-num)
  (%in=out flag-num))

(define-speedy-function %queue-peek (queue)
  "Dereference QUEUE's exit pointer"
  (svref queue (%queue-out queue)))

(define-speedy-function %queue-full-p (queue)
  "Checks whether QUEUE is effectively full"
  (= (logand (svref queue 0) #.*full-queue-mask*) 1))

(define-speedy-function %queue-empty-p (queue)
  "Checks whether QUEUE is effectively empty"
  (let ((flag (svref queue 0)))
    (and (%in=out flag)
         (= (logand flag #.*full-queue-mask*) 0))))

(define-speedy-function %queue-count (queue)
  "Returns QUEUE's effective length"
  ;; We start with the 'raw' length -- the difference between the pointers
  (let ((length (- (%queue-in queue) (%queue-out queue))))
    (declare (fixnum length))
    (cond ((plusp length) length)                         ; Raw length is OK
          ((or (minusp length)                            ; Entry pointer is before exit pointer,
               (%queue-full-p queue))                     ;   or the queue is full
           (the fixnum (+ length (%queue-length queue)))) ; Add the effective length
          (t 0))))                                        ; Queue is empty -- return zero

(define-speedy-function %next-index (current-index queue-real-length)
  (declare (fixnum current-index queue-real-length))
  (let ((new-index (1+ current-index)))                 ; Simply increment the index
    (declare (fixnum new-index))
    (the fixnum (if (= new-index queue-real-length)
                    1                                   ; Overflow to 1 if necessary
                    new-index))))

(define-speedy-function %out-index (flag)
  (declare (fixnum flag))
  (the fixnum (ash (logand flag #.*dequeue-mask*) #.(- *dequeue-start*))))

(define-speedy-function %in-index (flag)
  (declare (fixnum flag))
  (the fixnum (ash (logand flag #.*enqueue-mask*) #.(- *enqueue-start*))))

(define-speedy-function %fullp (flag)
  (declare (fixnum flag))
  (= (logand flag #.*full-queue-mask*) 1))

(define-speedy-function %decode-flag (flag)
  "(values dequeue-index enqueue-index full-status)"
  (declare (fixnum flag))
  (values (the fixnum (ash (logand flag #.*dequeue-mask*) #.(- *dequeue-start*))) ; out
          (the fixnum (ash (logand flag #.*enqueue-mask*) #.(- *enqueue-start*))) ; in
          (the fixnum (logand flag #.*full-queue-mask*))))                        ; full

(define-speedy-function %encode-flag (out in full)
  (declare (fixnum out in full))
  (the fixnum (+ (ash out #.*dequeue-start*)
                 (ash in  #.*enqueue-start*)
                 full)))

(define-speedy-function %enqueue (object queue)
  "Enqueue OBJECT and increment QUEUE's entry pointer."
  (loop (let ((old-flag (the fixnum (svref queue 0))))
          (multiple-value-bind (old-out old-in old-full) (%decode-flag old-flag)
            (if (= old-full 1) ; test if queue is full
                ;; full, enqueue will overflow, cas to make sure it's really full
                (when (atomics:cas (svref queue 0) old-flag old-flag) ; enqueue to a full queue is idempotent
                  (return #.*overflow-flag*))
                ;; not full, enqueue will success if cas return true.
                (let* ((new-in   (%next-index old-in (length (the (simple-vector *) queue))))
                       (new-full (if (= new-in old-out) 1 0))
                       (new-flag (%encode-flag old-out new-in new-full)))
                  (when (atomics:cas (svref queue 0) old-flag new-flag)
                    (return (setf (svref queue old-in) object)))))))))

(define-speedy-function %dequeue (queue keep-in-queue-p)
  "Sets QUEUE's tail to QUEUE, increments QUEUE's tail pointer, and returns the previous tail ref"
  (loop (let ((old-flag (the fixnum (svref queue 0))))
          (multiple-value-bind (old-out old-in old-full) (%decode-flag old-flag)
            (if (and (= old-out old-in) (= old-full 0)) ; test if queue is empty
                ;; empty, dequeue will underflow, cas to make sure it's really empty
                (when (atomics:cas (svref queue 0) old-flag old-flag) ; dequeue to an empty queue is idempotent
                  (return #.*underflow-flag*))
                ;; not empty, dequeue will success if cas return true
                (let* ((new-out (if (= (1+ old-out) (length (the (simple-vector *) queue)))
                                    1
                                    (1+ old-out)))
                       (new-flag (%encode-flag new-out old-in 0)))
                  (when (atomics:cas (svref queue 0) old-flag new-flag)
                    (let ((result (svref queue old-out)))
                      (unless keep-in-queue-p
                        (setf (svref queue old-out) nil))
                      (return result)))))))))

(define-speedy-function %queue-flush (queue)
  (loop for old-flag = (the fixnum (svref queue 0))
        with new-flag = #.(+ (ash 1 *dequeue-start*) (ash 1 #.*enqueue-start*))
        until (atomics:cas (svref queue 0) old-flag new-flag)
        finally (return queue)))

;;; Now that all the backend functions are defined, we can define the API:

(defun make-queue (size)
  "Makes a queue of maximum size SIZE"
  (declare (fixnum size))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (<= 2 size #.*max-queue-length*)
      (%make-queue size)
      (if (> size #.*max-queue-length*)
          (progn (warn "The size ~d exceeds the limit, will make a new queue with max size ~d" size #.*max-queue-length*)
                 (%make-queue #.*max-queue-length*))
          (error 'queue-length-error :attempted-length size))))

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
  (if (%queue-empty-p queue)
      (values nil nil)
      (values (%queue-peek queue) t)))

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
                   (coerce (subseq queue 1 in) 'list))))))

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
                (find item queue :start 1 :end in :key key :test test))))))

(defun queue-flush (queue)
  "Make `queue' empty"
  (%queue-flush queue))


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

#|
(defparameter *times* (loop for i from 3 to 8
                            collect (expt 10 i)))

(dolist (num *times*) ; about 15s in sbcl for 10^9
  (format t "LIFO queue, unsafe push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((queue (make-queue num)))
    (time (progn
            (dotimes (i num)
              (enqueue-unsafe i queue))
            (dotimes (i num)
              (dequeue-unsafe queue nil))))))

(dolist (num *times*) ; about 17s in sbcl for 10^9
  (format t "LIFO queue, unsafe push+pop, with consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((queue (make-queue num)))
          (dotimes (i num)
            (enqueue-unsafe i queue))
          (dotimes (i num)
            (dequeue-unsafe queue nil)))))

(dolist (num *times*) ; about 26s in sbcl for 10^9
  (format t "LIFO queue, safe push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((queue (make-queue num)))
    (time (progn
            (dotimes (i num)
              (enqueue i queue))
            (dotimes (i num)
              (dequeue queue nil))))))

(dolist (num *times*) ; about 28s in sbcl for 10^9
  (format t "LIFO queue, safe push+pop, without consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (time (let ((queue (make-queue num)))
          (dotimes (i num)
            (enqueue i queue))
          (dotimes (i num)
            (dequeue queue nil)))))

#+:sbcl
(dolist (num *times*) ; about 3.3s in sbcl for 10^8, failed for 10^9
  (format t "sb-concurrency's Singly-linked queue, safe push+pop, with consing timed: 10^~d times.~%" (log num 10))
  (sb-ext:gc :full t)
  (let ((queue (sb-concurrency:make-queue)))
    (time (progn
            (dotimes (i num)
              (sb-concurrency:enqueue i queue))
            (dotimes (i num)
              (sb-concurrency:dequeue queue))))))
|#





(defpackage #:cl-speedy-queue-safe-tests
  (:use #:cl #:parachute #:cl-speedy-queue-safe)
  (:export #:test))

(in-package :cl-speedy-queue-safe-tests)

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

(defun make-random-list (len &optional (max 5))
  (loop for i below len
        collect (random max)))

(define-test speedy-queue-safe)

(define-test speedy-queue-safe-single-thread :parent speedy-queue-safe
  (let* ((len 20)
         (queue (cl-speedy-queue-safe:make-queue len))
         (lst (loop for i from 0 below len collect i)))
    (is = 0 (cl-speedy-queue-safe:queue-count queue))
    (is = len (cl-speedy-queue-safe:queue-length queue))
    (is-values (cl-speedy-queue-safe:queue-peek queue) (eql nil) (eql nil))
    (false (cl-speedy-queue-safe:queue-full-p queue))
    (true (cl-speedy-queue-safe:queue-empty-p queue))
    (is equal nil (cl-speedy-queue-safe:queue-to-list queue))
    (is eql nil (cl-speedy-queue-safe:queue-find (1+ len) queue))
    (is eql nil (cl-speedy-queue-safe:queue-find nil queue))
    ;;(is eql nil (cl-speedy-queue-safe:queue-find cl-speedy-queue-safe::queue-sentinel queue))
    (loop for item in lst
          for idx from 0
          for count from 1
          do (progn (finish (cl-speedy-queue-safe:enqueue item queue))
                    (is = count (cl-speedy-queue-safe:queue-count queue))
                    (is = len (cl-speedy-queue-safe:queue-length queue))
                    (is-values (cl-speedy-queue-safe:queue-peek queue) (eql (first lst)) (eql t))
                    (if (= count len)
                        (true (cl-speedy-queue-safe:queue-full-p queue))
                        (false (cl-speedy-queue-safe:queue-full-p queue)))
                    (false (cl-speedy-queue-safe:queue-empty-p queue))
                    (is equal (subseq lst 0 count) (cl-speedy-queue-safe:queue-to-list queue))
                    (is eql nil (cl-speedy-queue-safe:queue-find (1+ len) queue))
                    (is eql nil (cl-speedy-queue-safe:queue-find nil queue))
                    ;;(is eql nil (cl-speedy-queue-safe:queue-find cl-speedy-queue-safe::queue-sentinel queue))
                    (loop for element in (subseq lst 0 count)
                          do (progn (is = element (cl-speedy-queue-safe:queue-find element queue))))
                    (loop for element in (subseq lst count len)
                          do (progn (is eql nil (cl-speedy-queue-safe:queue-find element queue))))
                    ))
    (loop for idx from 0 below len
          for count from 1
          do (progn (finish (cl-speedy-queue-safe:dequeue queue))
                    (is = (- len count) (cl-speedy-queue-safe:queue-count queue))
                    (is = len (cl-speedy-queue-safe:queue-length queue))
                    (if (= count len)
                        (is-values (cl-speedy-queue-safe:queue-peek queue) (eql nil) (eql nil))
                        (is-values (cl-speedy-queue-safe:queue-peek queue) (eql (nth count lst)) (eql t)))
                    (false (cl-speedy-queue-safe:queue-full-p queue))
                    (if (= count len)
                        (true (cl-speedy-queue-safe:queue-empty-p queue))
                        (false (cl-speedy-queue-safe:queue-empty-p queue)))
                    (is equal (subseq lst count len) (cl-speedy-queue-safe:queue-to-list queue))
                    (is eql nil (cl-speedy-queue-safe:queue-find (1+ len) queue))
                    (is eql nil (cl-speedy-queue-safe:queue-find nil queue))
                    ;;(is eql nil (cl-speedy-queue-safe:queue-find cl-speedy-queue::queue-sentinel queue))
                    (loop for element in (subseq lst count len)
                          do (progn (is = element (cl-speedy-queue-safe:queue-find element queue))))
                    (loop for element in (subseq lst 0 count)
                          do (progn (is eql nil (cl-speedy-queue-safe:queue-find element queue))))
      ))))

(defparameter *enqueue-sum* (make-atomic 0))
(defparameter *dequeue-sum* (make-atomic 0))

(define-test speedy-queue-safe-dequeue-enqueue-mixed-threads-kept :parent speedy-queue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i 10000);*loop-test-times*)
    (when (mod (1+ i) 1000) #+sbcl (sb-ext:gc :full t) #+ccl (ccl:gc))
    (setf *enqueue-sum* (make-atomic 0))
    (setf *dequeue-sum* (make-atomic 0))
    (let* ((n (+ 10 (random 10))) ; queue length
           (queue (cl-speedy-queue-safe:make-queue n))
           (push-threads nil)
           (pop-threads nil)
           (k (random n)) ; fill num
           (lst (make-random-list k))
           (total (apply #'+ lst)))
      (assert (<= (length lst) (cl-speedy-queue-safe:queue-length queue)))
      (dolist (element lst)
        (let ((ele element)) ; make a bind as the var of element will change and will affect among the threads
          (push (bt:make-thread #'(lambda ()
                                    (let ((res (cl-speedy-queue-safe:enqueue ele queue)))
                                      (if (integerp res)
                                          (atomic-incf (atomic-place *enqueue-sum*) res)
                                          (format t "~&To enqueue: ~d, ret: ~d, queue: ~d~%" ele res queue)))))
                push-threads)))

      (dolist (element lst)
        (push (bt:make-thread #'(lambda ()
                                  (let ((res (cl-speedy-queue-safe:dequeue queue t)))
                                    (if (integerp res)
                                        (atomic-incf (atomic-place *dequeue-sum*) res)
                                        (format t "~&Dequeue return: ~d, queue: ~d~%" res queue)))))
              pop-threads))

      (dolist (thread push-threads)
        (bt:join-thread thread))
      (dolist (thread pop-threads)
        (bt:join-thread thread))

      (is = total (atomic-place *enqueue-sum*))
      (is = total (+ (atomic-place *dequeue-sum*)
                     (apply #'+ (cl-speedy-queue-safe:queue-to-list queue))))

      (unless (cl-speedy-queue-safe:queue-empty-p queue)
        (format t "~&queue not empty: ~d~%elements: ~d~%" queue (cl-speedy-queue-safe:queue-to-list queue)))
      (unless (= total (+ (atomic-place *dequeue-sum*)
                          (apply #'+ (cl-speedy-queue-safe:queue-to-list queue))))
        (format t "~&~%List: ~d~%" lst)
        (format t "~&items num: ~d, enqueue sum: ~d, expect sum: ~d~%" k (atomic-place *enqueue-sum*) total)
        (format t "~&Queue count: ~d~%" (cl-speedy-queue-safe:queue-count queue))
        (format t "~&Queue: ~d~%~%" queue))
      )))
