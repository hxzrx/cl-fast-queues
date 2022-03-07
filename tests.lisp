
(defpackage #:cl-fast-queues-tests
  (:use #:cl #:parachute #:cl-fast-queues)
  (:export nil))


(in-package :cl-fast-queues-tests)

(defun nshuffle (sequence)
  "Knuth shuffle. https://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp"
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)


(define-test cl-fast-queues-tests)

(define-test speedy-queue :parent cl-fast-queues-tests)
(define-test list-queue :parent cl-fast-queues-tests)

(define-test unsafe-queues :parent cl-fast-queues-tests)
(define-test unsafe-fifo :parent unsafe-queues)
(define-test unsafe-lifo :parent unsafe-queues)

(define-test safe-queues :parent cl-fast-queues-tests)
(define-test safe-fifo :parent safe-queues)
(define-test safe-lifo :parent safe-queues)

(defparameter *loop-test-times* 100)


;;;; -----------------------------------------------------------

;;;; speedy-queue.lisp

(define-test speedy-queue-flush :parent speedy-queue
  (dotimes (i *loop-test-times*)
    (let* ((count (1+ (random 10)))
           (queue (cl-speedy-queue:make-queue count))
           (most-enq (random count)))
      (dotimes (i most-enq)
        (cl-speedy-queue:enqueue 0 queue))
      (finish (cl-speedy-queue:queue-flush queue))
      (is eq t (cl-speedy-queue:queue-empty-p queue))
      (dotimes (i most-enq)
        (cl-speedy-queue:enqueue 0 queue))
      (finish (cl-speedy-queue:queue-flush queue))
      (is eq t (cl-speedy-queue:queue-empty-p queue)))))


;;;; list-queue.lisp

(define-test list-queue-flush :parent list-queue
  (dotimes (i *loop-test-times*)
    (let* ((count (1+ (random 10)))
           (queue (cl-fast-queues::%make-list-queue))
           (most-enq (random count)))
      (dotimes (i most-enq)
        (cl-fast-queues::%list-queue-enqueue 0 queue))
      (finish (cl-fast-queues::%list-queue-flush queue))
      (is eq t (cl-fast-queues::%list-queue-empty-p queue))
      (dotimes (i most-enq)
        (cl-fast-queues::%list-queue-enqueue 0 queue))
      (finish (cl-fast-queues::%list-queue-flush queue))
      (is eq t (cl-fast-queues::%list-queue-empty-p queue)))))

(define-test list-queue-find :parent list-queue
  (dotimes (i *loop-test-times*)
    (let* ((count (1+ (random 10)))
           (queue (cl-fast-queues::%make-list-queue))
           (most-enq count)
           (obj-in (random count))
           (obj-out (+ count (random 10))))
      (dotimes (i most-enq)
        (cl-fast-queues::%list-queue-enqueue i queue))
      (is eq t (cl-fast-queues::%list-queue-find obj-in queue))
      (is eq nil (cl-fast-queues::%list-queue-find obj-out queue))
      (is eq nil (cl-fast-queues::%list-queue-find nil queue)))))


;;;; unsafe-queues.lisp

;;; ------- unsafe-fifo -------

(define-test make-unsafe-fifo :parent unsafe-fifo
  (finish (make-unsafe-fifo))
  (finish (make-unsafe-fifo :init-length 1))
  (fail (make-unsafe-fifo :init-length 0)))

(define-test %singularp :parent cl-fast-queues-tests
  (is eql t (cl-fast-queues::%singularp nil))
  (is eql t (cl-fast-queues::%singularp '(0)))
  (is eql t (cl-fast-queues::%singularp '(nil)))
  (is eql nil (cl-fast-queues::%singularp '(0 1))))

(define-test unsafe-fifo-queue-count-0 :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-count-int :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-count-str :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-count-nil :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-empty-p :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (enqueue nil queue)
        (is eq nil (queue-empty-p queue)))
      (dotimes (j count)
        (dequeue queue))
      (is eq t (queue-empty-p queue))
      (dotimes (k (random 10))
        (dequeue queue)
        (is eq t (queue-empty-p queue))))))

(define-test unsafe-fifo-enqueue :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test unsafe-fifo-queue-peek :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (queue-peek queue) (eql nil) (eql nil))
      (enqueue first-item queue)
      (is equal first-item (queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (enqueue item queue)
          (is equal first-item (queue-peek queue)))))))

(define-test unsafe-fifo-dequeue :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           #+:ignore(items-rev (reverse items)))
      (is eq cl-speedy-queue:*underflow-flag* (dequeue queue))
      (dolist (item items)
        (enqueue item queue))
      (dolist (item items)
        (is = item (dequeue queue)))
      (is eq cl-speedy-queue:*underflow-flag* (dequeue queue)))))

(define-test unsafe-fifo-queue-find :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let ((find-items (loop for i below (1+ (random 10)) collect (random 10)))
          (other-items (loop for i below (1+ (random 10)) collect (+ 100 (random 10))))
          (queue (make-unsafe-fifo :init-length (1+ (random 10)))))
      (dolist (item find-items) ; find in empty queue
        (false (queue-find item queue)))
      (dolist (item (append find-items other-items)) ; enqueue
        (enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (queue-find item queue)))
      (dolist (item other-items) ; find queue
        (true (queue-find item queue)))
      (dolist (item find-items) ; dequeue
        (dequeue queue))
      (dolist (item find-items) ; find dequeued items
        (false (queue-find item queue)))
      (dolist (item other-items) ; empty queue
        (dequeue queue))
      (dolist (item (nshuffle (append find-items other-items))) ; enqueue random list
        (enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (queue-find item queue))))))

(define-test unsafe-fifo-queue-flush :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let* ((count (random 20))
           (queue (make-unsafe-fifo :init-length (1+ (random 10)))))
      (finish (cl-fast-queues::queue-flush queue))
      (dotimes (i count)
        (enqueue i queue))
      (finish (cl-fast-queues::queue-flush queue))
      (is eql t (queue-empty-p queue))
      (dotimes (i count)
        (enqueue i queue))
      (finish (cl-fast-queues::queue-flush queue))
      (is eql t (queue-empty-p queue)))))

(define-test unsafe-fifo-queue<->list :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let ((items (loop for i below (1+ (random 50)) collect (random 10)))
          (queue1 (make-unsafe-fifo :init-length (1+ (random 50))))
          (queue2 nil))
      (dolist (item items)
        (enqueue item queue1))
      (is equal items (queue-to-list queue1))
      (setf queue2 (list-to-queue items :unsafe-fifo))
      #+:ignore(is equal items (queue-to-list queue2)))))


;;; ------- unsafe-lifo -------

(define-test make-unsafe-lifo :parent unsafe-lifo
  (finish (make-unsafe-lifo))
  (finish (make-unsafe-lifo :init-length 1))
  (fail (make-unsafe-lifo :init-length 0)))

(define-test unsafe-lifo-queue-count-0 :parent unsafe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-count-int :parent unsafe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-count-str :parent unsafe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-count-nil :parent unsafe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-empty-p :parent unsafe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (enqueue nil queue)
        (is eq nil (queue-empty-p queue)))
      (dotimes (j count)
        (dequeue queue))
      (is eq t (queue-empty-p queue))
      (dotimes (k (random 10))
        (dequeue queue)
        (is eq t (queue-empty-p queue))))))

(define-test unsafe-lifo-enqueue :parent unsafe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test unsafe-lifo-queue-peek :parent unsafe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (queue-peek queue) (eql nil) (eql nil))
      (enqueue first-item queue)
      (is equal first-item (queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (enqueue item queue)
          (is = item (queue-peek queue)))))))

(define-test unsafe-lifo-dequeue :parent unsafe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           (items-rev (reverse items)))
      (is eq cl-speedy-lifo::*underflow-flag* (dequeue queue))
      (dolist (item items)
        (enqueue item queue))
      (dolist (item items-rev)
        (is = item (dequeue queue)))
      (is eq cl-speedy-lifo::*underflow-flag* (dequeue queue)))))

(define-test unsafe-lifo-queue-find :parent unsafe-lifo
  (dotimes (i *loop-test-times*)
    (let ((find-items (loop for i below (1+ (random 10)) collect (random 10)))
          (other-items (loop for i below (1+ (random 10)) collect (+ 100 (random 10))))
          (queue (make-unsafe-lifo :init-length (1+ (random 10)))))
      (dolist (item find-items) ; find in empty queue
        (false (queue-find item queue)))
      (dolist (item (append other-items find-items)) ; enqueue, `find-items' euqueue last
        (enqueue item queue))
      (dolist (item find-items) ; find queue
                 (true (queue-find item queue)))
      (dolist (item other-items) ; find queue
        (true (queue-find item queue)))
      (dolist (item find-items) ; dequeue
        (dequeue queue))
      (dolist (item find-items) ; find dequeued items
        (false (queue-find item queue)))
      (dolist (item other-items) ; empty queue
        (dequeue queue))
      (dolist (item (nshuffle (append find-items other-items))) ; enqueue random list
        (enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (queue-find item queue))))))

(define-test unsafe-lifo-queue<->list :parent unsafe-lifo
  (dotimes (i *loop-test-times*)
    (let ((items (loop for i below (1+ (random 50)) collect (random 10)))
          (queue1 (make-unsafe-lifo :init-length (1+ (random 50))))
          (queue2 nil))
      (dolist (item items)
        (enqueue item queue1))
      (is equal (reverse items) (queue-to-list queue1))
      (setf queue2 (list-to-queue items :unsafe-lifo))
      (is equal (reverse items) (queue-to-list queue2)))))


;;;; -----------------------------------------------------------
;;;; safe-queues.lisp
;;;; All these tests are copied from the tests of the unsafe-queues for single thread testing
;;; ------- safe-fifo -------

(define-test make-safe-fifo :parent safe-fifo
  (finish (make-safe-fifo))
  (finish (make-safe-fifo :init-length 1))
  (finish (make-safe-fifo :waitp t))
  (finish (make-safe-fifo :init-length 1 :waitp t))
  (finish (make-safe-fifo :init-length 1 :waitp nil))
  (fail (make-safe-fifo :init-length 0)))

(define-test safe-fifo-queue-count-0 :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-count-int :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-count-str :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-count-nil :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-empty-p :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (enqueue nil queue)
        (is eq nil (queue-empty-p queue)))
      (dotimes (j count)
        (dequeue queue))
      (is eq t (queue-empty-p queue))
      (dotimes (k (random 10))
        (dequeue queue)
        (is eq t (queue-empty-p queue))))))

(define-test safe-fifo-enqueue :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test safe-fifo-queue-peek :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (queue-peek queue) (eql nil) (eql nil))
      (enqueue first-item queue)
      (is equal first-item (queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (enqueue item queue)
          (is equal first-item (queue-peek queue)))))))

(define-test safe-fifo-dequeue :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           #+:ignore(items-rev (reverse items)))
      (is eq cl-speedy-queue:*underflow-flag* (dequeue queue))
      (dolist (item items)
        (enqueue item queue))
      (dolist (item items)
        (is = item (dequeue queue)))
      (is eq cl-speedy-queue:*underflow-flag* (dequeue queue)))))

(define-test safe-fifo-queue-find :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((find-items (loop for i below (1+ (random 10)) collect (random 10)))
          (other-items (loop for i below (1+ (random 10)) collect (+ 100 (random 10))))
          (queue (make-safe-fifo :init-length (1+ (random 10)) :waitp nil)))
      (dolist (item find-items) ; find in empty queue
        (false (queue-find item queue)))
      (dolist (item (append find-items other-items)) ; enqueue
        (enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (queue-find item queue)))
      (dolist (item other-items) ; find queue
        (true (queue-find item queue)))
      (dolist (item find-items) ; dequeue
        (dequeue queue))
      (dolist (item find-items) ; find dequeued items
        (false (queue-find item queue)))
      (dolist (item other-items) ; empty queue
        (dequeue queue))
      (dolist (item (nshuffle (append find-items other-items))) ; enqueue random list
        (enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (queue-find item queue))))))

(define-test safe-fifo-queue<->list :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((items (loop for i below (1+ (random 50)) collect (random 10)))
          (queue1 (make-safe-fifo :init-length (1+ (random 50)) :waitp nil))
          (queue2 nil))
      (dolist (item items)
        (enqueue item queue1))
      (is equal items (queue-to-list queue1))
      (setf queue2 (list-to-queue items :safe-fifo))
      (is equal items (queue-to-list queue2)))))


;;; ------- safe-lifo -------

(define-test make-safe-lifo :parent safe-lifo
  (finish (make-safe-lifo))
  (finish (make-safe-lifo :init-length 1))
  (finish (make-safe-lifo :waitp t))
  (finish (make-safe-lifo :init-length 1 :waitp t))
  (finish (make-safe-lifo :init-length 1 :waitp nil))
  (fail (make-safe-lifo :init-length 0)))


(define-test safe-lifo-queue-count-0 :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-count-int :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-count-str :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-count-nil :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-empty-p :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (enqueue nil queue)
        (is eq nil (queue-empty-p queue)))
      (dotimes (j count)
        (dequeue queue))
      (is eq t (queue-empty-p queue))
      (dotimes (k (random 10))
        (dequeue queue)
        (is eq t (queue-empty-p queue))))))

(define-test safe-lifo-enqueue :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test safe-lifo-queue-peek :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (queue-peek queue) (eql nil) (eql nil))
      (enqueue first-item queue)
      (format t "enqueue finished.~%")
      (is equal first-item (queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (enqueue item queue)
          (is = item (queue-peek queue)))))))

(define-test safe-lifo-dequeue :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           (items-rev (reverse items)))
      (is eq cl-speedy-lifo::*underflow-flag* (dequeue queue))
      (dolist (item items)
        (enqueue item queue))
      (dolist (item items-rev)
        (is = item (dequeue queue)))
      (is eq cl-speedy-lifo::*underflow-flag* (dequeue queue)))))

(define-test safe-lifo-queue-find :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let ((find-items (loop for i below (1+ (random 10)) collect (random 10)))
          (other-items (loop for i below (1+ (random 10)) collect (+ 100 (random 10))))
          (queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil)))
      (dolist (item find-items) ; find in empty queue
        (false (queue-find item queue)))
      (dolist (item (append other-items find-items)) ; enqueue, `find-items' euqueue last
        (enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (queue-find item queue)))
      (dolist (item other-items) ; find queue
        (true (queue-find item queue)))
      (dolist (item find-items) ; dequeue
        (dequeue queue))
      (dolist (item find-items) ; find dequeued items
        (false (queue-find item queue)))
      (dolist (item other-items) ; empty queue
        (dequeue queue))
      (dolist (item (nshuffle (append find-items other-items))) ; enqueue random list
        (enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (queue-find item queue))))))

(define-test safe-lifo-queue<->list :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let ((items (loop for i below (1+ (random 50)) collect (random 10)))
          (queue1 (make-safe-lifo :init-length (1+ (random 50)) :waitp nil))
          (queue2 nil))
      (dolist (item items)
        (enqueue item queue1))
      (is equal (reverse items) (queue-to-list queue1))
      (setf queue2 (list-to-queue items :safe-lifo))
      (is equal (reverse items) (queue-to-list queue2)))))


;;; ------- tests in multi-threads ------

;;; safe-fifo, threads test

(define-test safe-fifo-enqueue-threads :parent safe-fifo
  "enqueue in threads, dequeue in the main thread"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp t))
          (items (loop for i below (random 100) collect (random 100)))
          (total 0))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1) ; make sure all threads exited
      (is = (length items) (queue-count queue))
      (dotimes (j (length items))
        (setf total (+ total (dequeue queue))))
      (is = total (apply #'+ items))
      (true (queue-empty-p queue)))))

(define-test safe-fifo-dequeue-threads-no-wait :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (apply #'+ items)))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = total
          (apply #'+
                 (loop for j below (length items)
                       collect (bt:join-thread
                                (bt:make-thread #'(lambda () (dequeue queue t)))))))
      (true (queue-empty-p queue)))))

(define-test safe-fifo-dequeue-threads-wait :parent safe-fifo
  "enqueue in threads, then dequeue in threads"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (apply #'+ items)))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = total
          (apply #'+
                 (loop for j below (length items)
                       collect (bt:join-thread
                                (bt:make-thread #'(lambda () (dequeue queue t)))))))
      (true (queue-empty-p queue)))))

(define-test safe-fifo-dequeue-threads-wait2 :parent safe-fifo
  "dequeue with waitp true in threads, then enqueue in threads"
  ;; ccl running failed
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total 0))
      (bt:make-thread
       #'(lambda ()
           (setf total
                 (apply #'+
                        (loop for j below (length items)
                              collect (bt:join-thread
                                       (bt:make-thread #'(lambda () (dequeue queue t)))))))))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = total (apply #'+ items))
      (true (queue-empty-p queue))
      )))

#+sbcl
(define-test safe-fifo-dequeue-threads-wait3 :parent safe-fifo
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (list 0)))
      (bt:make-thread
       #'(lambda ()
           (dolist (item items)
             (sb-ext:atomic-incf (car total)
                 (bt:join-thread
                  (bt:make-thread #'(lambda () (dequeue queue t))))))))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = (car total) (apply #'+ items))
      (true (queue-empty-p queue)))))

#+sbcl
(define-test safe-fifo-dequeue-threads-wait4 :parent safe-fifo
  "dequeue and atomic-incf in threads, enqueue in threads"
  ;; still failed after using bt's apiv2 if sole using (bt2:condition-wait cvar lock) in the def of dequeue,
  ;; but succeeded when I use a loop in the def of dequeue.
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((len (1+ (random 10)))
           (queue (make-safe-fifo :init-length len :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (list 0)))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (sb-ext:atomic-incf (car total)
                                (dequeue queue t)))))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = (car total) (apply #'+ items))
      (when (/= (car total) (apply #'+ items))
        (format t "init-len: ~d~%items:~%~d~%queue:~%~d~%" len items queue))
      (true (queue-empty-p queue)))))


;;; safe-lifo, threads test

(define-test safe-lifo-enqueue-threads :parent safe-lifo
  "enqueue in threads, dequeue in the main thread"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp t))
          (items (loop for i below (random 100) collect (random 100)))
          (total 0))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1) ; make sure all threads exited
      (is = (length items) (queue-count queue))
      (dotimes (j (length items))
        (setf total (+ total (dequeue queue))))
      (is = total (apply #'+ items))
      (true (queue-empty-p queue)))))

(define-test safe-lifo-dequeue-threads-no-wait :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp nil))
           (items (loop for i below (random 100) collect (random 100)))
           (total (apply #'+ items)))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = total
          (apply #'+
                 (loop for j below (length items)
                       collect (bt:join-thread
                                (bt:make-thread #'(lambda () (dequeue queue t)))))))
      (true (queue-empty-p queue)))))

(define-test safe-lifo-dequeue-threads-wait :parent safe-lifo
  "enqueue in threads, then dequeue in threads with waitp true"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (apply #'+ items)))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = total
          (apply #'+
                 (loop for j below (length items)
                       collect (bt:join-thread
                                (bt:make-thread #'(lambda () (dequeue queue t)))))))
      (true (queue-empty-p queue)))))

#+sbcl
(define-test safe-lifo-dequeue-threads-wait2-0 :parent safe-lifo
  "dequeue in threads with waitp true, then enqueue in threads"
  ;; stil failed in several cases
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (list 0)))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (sb-ext:atomic-incf (car total)
                                (dequeue queue t))
                            :name "dequeue thread")))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))
                        :name "enqueue thread"))
      (sleep 0.1)
      (is = (car total) (apply #'+ items))
      (true (queue-empty-p queue)))))

(defparameter *dequeue-list* (list))
(defparameter *enqueue-list* (list))
#+sbcl
(define-test safe-lifo-dequeue-threads-wait2 :parent safe-lifo
  "dequeue with waitp true in threads, then enqueue in threads"
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 5)) :waitp t))
           (items (loop for i below (random 20) collect (random 10)))
           (items-sum (apply #'+ items)))
      (setf *dequeue-list* (list))
      (setf *enqueue-list* (list))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (sb-ext:atomic-push (dequeue queue nil)
                                                *dequeue-list*))))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (sb-ext:atomic-push (enqueue item queue)
                                                *enqueue-list*))))
      (sleep 0.1)
      (is = items-sum (apply #'+ *enqueue-list*))
      (is = items-sum (apply #'+ *dequeue-list*))
      ;;(when (not (= items-sum (apply #'+ *dequeue-list*)))
      ;;(format t "items: ~d~%enque: ~d~%deque: ~d~%~%" items *enqueue-list* *dequeue-list*)
      #+:ignore(unless (and (every #'integerp *dequeue-list*)
                            (every #'integerp *enqueue-list*))
                 (sleep 0.2)
                 (format t "enque again: ~d~%deque again: ~d~%" *enqueue-list* *dequeue-list*))
      (true (queue-empty-p queue))
      )))

#+sbcl
(define-test safe-lifo-dequeue-threads-wait3 :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (list 0)))
      (bt:make-thread
       #'(lambda ()
           (dolist (item items)
             (sb-ext:atomic-incf (car total)
                 (bt:join-thread
                  (bt:make-thread #'(lambda () (dequeue queue t))))))))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = (car total) (apply #'+ items))
      (true (queue-empty-p queue)))))

#+sbcl
(define-test safe-lifo-dequeue-threads-wait4 :parent safe-lifo
  "dequeue and atomic-incf in threads, enqueue in threads"
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)) :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (list 0)))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (sb-ext:atomic-incf (car total)
                                (dequeue queue t)))))
      (dolist (item items)
        (bt:make-thread #'(lambda ()
                            (enqueue item queue))))
      (sleep 0.1)
      (is = (car total) (apply #'+ items))
      (true (queue-empty-p queue)))))
