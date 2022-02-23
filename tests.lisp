
(defpackage #:cl-fast-queues-tests
  (:use #:cl #:parachute #:cl-fast-queues)
  (:export nil))


(in-package :cl-fast-queues-tests)

(define-test cl-fast-queues-tests)

(define-test unsafe-queues :parent cl-fast-queues-tests)
(define-test unsafe-fifo :parent unsafe-queues)
(define-test unsafe-lifo :parent unsafe-queues)

(define-test safe-queues :parent cl-fast-queues-tests)
(define-test safe-fifo :parent safe-queues)
(define-test safe-lifo :parent safe-queues)

(defparameter *loop-test-times* 100)


;;;; -----------------------------------------------------------
;;;; unsafe-queues.lisp

;;; ------- unsafe-fifo -------

(define-test make-unsafe-fifo :parent unsafe-fifo
  (finish (make-unsafe-fifo))
  (finish (make-unsafe-fifo :init-length 1))
  (finish (make-unsafe-fifo :enlarge-size 1))
  (finish (make-unsafe-fifo :enlarge-threshold 1))
  (fail (make-unsafe-fifo :init-length 0))
  (fail (make-unsafe-fifo :enlarge-threshold 1.00001))
  (fail (make-unsafe-fifo :enlarge-size 0.999999)))

(define-test %singularp :parent cl-fast-queues-tests
  (is eql t (cl-fast-queues::%singularp nil))
  (is eql t (cl-fast-queues::%singularp '(0)))
  (is eql t (cl-fast-queues::%singularp '(nil)))
  (is eql nil (cl-fast-queues::%singularp '(0 1))))

(define-test unsafe-fifo-queue-count-0 :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-count-int :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-count-str :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-count-nil :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test unsafe-fifo-queue-empty-p :parent unsafe-fifo
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
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test unsafe-fifo-queue-peek :parent unsafe-fifo
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


;;; ------- unsafe-lifo -------

(define-test make-unsafe-lifo :parent unsafe-lifo
  (finish (make-unsafe-lifo))
  (finish (make-unsafe-lifo :init-length 1))
  (finish (make-unsafe-lifo :enlarge-size 1))
  (finish (make-unsafe-lifo :enlarge-threshold 1))
  (fail (make-unsafe-lifo :init-length 0))
  (fail (make-unsafe-lifo :enlarge-threshold 1.00001))
  (fail (make-unsafe-lifo :enlarge-size 0.999999)))

(define-test unsafe-lifo-queue-count-0 :parent unsafe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-count-int :parent unsafe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-count-str :parent unsafe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-count-nil :parent unsafe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test unsafe-lifo-queue-empty-p :parent unsafe-lifo
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
  (dotimes (i *loop-test-times*)
    (let ((queue (make-unsafe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test unsafe-lifo-queue-peek :parent unsafe-lifo
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


;;;; -----------------------------------------------------------
;;;; safe-queues.lisp
;;;; All these tests are copied from the tests of the unsafe-queues for single thread testing
;;; ------- safe-fifo -------

(define-test make-safe-fifo :parent safe-fifo
  (finish (make-safe-fifo))
  (finish (make-safe-fifo :init-length 1))
  (finish (make-safe-fifo :enlarge-size 1))
  (finish (make-safe-fifo :enlarge-threshold 1))
  (fail (make-safe-fifo :init-length 0))
  (fail (make-safe-fifo :enlarge-threshold 1.00001))
  (fail (make-safe-fifo :enlarge-size 0.999999)))

(define-test safe-fifo-queue-count-0 :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-count-int :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-count-str :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-count-nil :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test safe-fifo-queue-empty-p :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
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
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test safe-fifo-queue-peek :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
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
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10))))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           #+:ignore(items-rev (reverse items)))
      (is eq cl-speedy-queue:*underflow-flag* (dequeue queue))
      (dolist (item items)
        (enqueue item queue))
      (dolist (item items)
        (is = item (dequeue queue)))
      (is eq cl-speedy-queue:*underflow-flag* (dequeue queue)))))


;;; ------- safe-lifo -------

(define-test make-safe-lifo :parent safe-lifo
  (finish (make-unsafe-lifo))
  (finish (make-unsafe-lifo :init-length 1))
  (finish (make-unsafe-lifo :enlarge-size 1))
  (finish (make-unsafe-lifo :enlarge-threshold 1))
  (fail (make-unsafe-lifo :init-length 0))
  (fail (make-unsafe-lifo :enlarge-threshold 1.00001))
  (fail (make-unsafe-lifo :enlarge-size 0.999999)))

(define-test safe-lifo-queue-count-0 :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue 0 queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-count-int :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (random 10) queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-count-str :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue (write-to-string (random 10)) queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-count-nil :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (is = 0 (queue-count queue))
      (is eq t (queue-empty-p queue))
      (dotimes (j count)
        (enqueue nil queue))
      (is = count (queue-count queue)))))

(define-test safe-lifo-queue-empty-p :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
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
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (enqueue item queue)))))))

(define-test safe-lifo-queue-peek :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (queue-peek queue) (eql nil) (eql nil))
      (enqueue first-item queue)
      (is equal first-item (queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (enqueue item queue)
          (is = item (queue-peek queue)))))))

(define-test safe-lifo-dequeue :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10))))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           (items-rev (reverse items)))
      (is eq cl-speedy-lifo::*underflow-flag* (dequeue queue))
      (dolist (item items)
        (enqueue item queue))
      (dolist (item items-rev)
        (is = item (dequeue queue)))
      (is eq cl-speedy-lifo::*underflow-flag* (dequeue queue)))))


;;; ------- tests in multi-threads ------

(define-test safe-fifo-enqueue-threads :parent safe-fifo
  "enqueue in threads, dequeue in the main thread"
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-fifo :init-length (1+ (random 10))))
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
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10))))
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
                                (bt:make-thread #'(lambda () (dequeue queue :keep-in-queue-p t :waitp nil)))))))
      (true (queue-empty-p queue)))))

(define-test safe-fifo-dequeue-threads-wait :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10))))
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
                                (bt:make-thread #'(lambda () (dequeue queue :keep-in-queue-p t :waitp t)))))))
      (true (queue-empty-p queue)))))


;;; safe-lifo

(define-test safe-lifo-enqueue-threads :parent safe-lifo
  "enqueue in threads, dequeue in the main thread"
  (dotimes (i *loop-test-times*)
    (let ((queue (make-safe-lifo :init-length (1+ (random 10))))
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
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10))))
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
                                (bt:make-thread #'(lambda () (dequeue queue :keep-in-queue-p t :waitp nil)))))))
      (true (queue-empty-p queue)))))

(define-test safe-lifo-dequeue-threads-wait :parent safe-lifo
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10))))
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
                                (bt:make-thread #'(lambda () (dequeue queue :keep-in-queue-p t :waitp t)))))))
      (true (queue-empty-p queue)))))
