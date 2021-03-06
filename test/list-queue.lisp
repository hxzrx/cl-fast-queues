(in-package :cl-fast-queues-tests)

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

#+sbcl
(define-test list-queue-find :parent list-queue
  (dotimes (i *loop-test-times*)
    (let* ((count (1+ (random 10)))
           (queue (cl-fast-queues::%make-list-queue))
           (most-enq count)
           (obj-in (random count))
           (obj-out (+ count (random 10))))
      (dotimes (i most-enq)
        (cl-fast-queues::%list-queue-enqueue i queue))
      (is eq obj-in (cl-fast-queues::%list-queue-find obj-in queue))
      (is eq nil (cl-fast-queues::%list-queue-find obj-out queue))
      (is eq nil (cl-fast-queues::%list-queue-find nil queue)))))
