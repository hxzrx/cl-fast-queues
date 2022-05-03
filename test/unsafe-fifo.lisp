(in-package :cl-fast-queues-tests)

;;; ------- unsafe-fifo -------

(define-test make-unsafe-fifo :parent unsafe-fifo
  (finish (make-unsafe-fifo))
  (finish (make-unsafe-fifo :init-length 1))
  (fail (make-unsafe-fifo :init-length 0)))

(define-test %singularp :parent cl-fast-queues-tests
  (is eql nil (cl-fast-queues::%singularp nil))
  (is eql t   (cl-fast-queues::%singularp '(0)))
  (is eql t   (cl-fast-queues::%singularp '(nil)))
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
