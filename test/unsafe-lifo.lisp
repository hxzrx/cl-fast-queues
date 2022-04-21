(in-package :cl-fast-queues-tests)

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

(define-test unsafe-lifo-queue-flush :parent unsafe-lifo
  (dotimes (i *loop-test-times*)
    (let* ((count (random 20))
           (queue (make-unsafe-lifo :init-length (1+ (random 10)))))
      (finish (cl-fast-queues::queue-flush queue))
      (dotimes (i count)
        (enqueue i queue))
      (finish (cl-fast-queues::queue-flush queue))
      (is eql t (queue-empty-p queue))
      (dotimes (i count)
        (enqueue i queue))
      (finish (cl-fast-queues::queue-flush queue))
      (is eql t (queue-empty-p queue)))))

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
