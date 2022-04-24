;;;; The safe-fast-fifo and safe-fast-lifo structs are reconstructed
;;;; and the corresponding safe-queues test cases are rewritten,
;;;; since the new structs do not have waitp slot anymore,
;;;; many test cases about threads were deprecated and the some new ones were made.

(in-package :cl-fast-queues-tests)

(defparameter *enqueue-sum* (make-atomic 0))
(defparameter *dequeue-sum* (make-atomic 0))


;;;; -----------------------------------------------------------
;;;; safe-queues.lisp
;;;; All these tests are copied from the tests of the unsafe-queues for single thread testing
;;; ------- safe-fifo -------

(define-test make-safe-fifo-exp :parent safe-fifo-exp
  (finish (cl-fast-queues-exp:make-safe-fifo))
  (finish (cl-fast-queues-exp:make-safe-fifo :init-length 2))
  (fail (cl-fast-queues-exp:make-safe-fifo :init-length 1))
  (fail (cl-fast-queues-exp:make-safe-fifo :init-length 0)))

(define-test safe-fifo-exp-queue-count-0 :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue 0 queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-fifo-exp-queue-count-int :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue (random 10) queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-fifo-exp-queue-count-str :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue (write-to-string (random 10)) queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-fifo-exp-queue-count-nil :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue nil queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-fifo-exp-queue-empty-p :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((len 2);(1+ (random 10)))
           (queue (cl-fast-queues-exp:make-safe-fifo :init-length len)) ; :waitp nil))
           (count 5));(random 100)))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue nil queue)
        (is eq nil (cl-fast-queues-exp:queue-empty-p queue)))
      (dotimes (j count)
        (cl-fast-queues-exp:dequeue queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (k (random 10))
        (cl-fast-queues-exp:dequeue queue)
        (is eq t (cl-fast-queues-exp:queue-empty-p queue))))))

(define-test safe-fifo-exp-enqueue :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (cl-fast-queues-exp:enqueue item queue)))))))

(define-test safe-fifo-exp-queue-peek :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i 1);*loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (cl-fast-queues-exp:queue-peek queue) (eql nil) (eql nil))
      (cl-fast-queues-exp:enqueue first-item queue)
      (is equal first-item (cl-fast-queues-exp:queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (cl-fast-queues-exp:enqueue item queue)
          (is equal first-item (cl-fast-queues-exp:queue-peek queue)))))))

(define-test safe-fifo-exp-dequeue :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp nil))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           #+:ignore(items-rev (reverse items)))
      (is eq cl-speedy-queue:*underflow-flag* (cl-fast-queues-exp:dequeue queue))
      (dolist (item items)
        (cl-fast-queues-exp:enqueue item queue))
      (dolist (item items)
        (is = item (cl-fast-queues-exp:dequeue queue)))
      (is eq cl-speedy-queue:*underflow-flag* (cl-fast-queues-exp:dequeue queue)))))

(define-test safe-fifo-exp-queue-find :parent safe-fifo-exp
  (dotimes (i *loop-test-times*)
    (let* ((find-items (loop for i below (1+ (random 10)) collect (random 10)))
           (other-items (loop for i below (1+ (random 10)) collect (+ 100 (random 10))))
           (len (+ 2 (random 10)))
           (queue (cl-fast-queues-exp:make-safe-fifo :init-length len))) ; :waitp nil)))
      (dolist (item find-items) ; find in empty queue
        (false (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item (append find-items other-items)) ; enqueue
        (cl-fast-queues-exp:enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item other-items) ; find queue
        (true (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item find-items) ; dequeue
        (cl-fast-queues-exp:dequeue queue))
      (dolist (item find-items) ; find dequeued items
        (false (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item other-items) ; empty queue
        (cl-fast-queues-exp:dequeue queue))
      (dolist (item (nshuffle (append find-items other-items))) ; enqueue random list
        (cl-fast-queues-exp:enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (cl-fast-queues-exp:queue-find item queue))))))

(define-test safe-fifo-exp-queue-flush :parent safe-fifo-exp
  (dotimes (i *loop-test-times*)
    (let* ((count (random 20))
           (queue (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 10)))))
      (finish (cl-fast-queues-exp:queue-flush queue))
      (dotimes (i count)
        (cl-fast-queues-exp:enqueue i queue))
      (finish (cl-fast-queues-exp:queue-flush queue))
      (is eql t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (i count)
        (cl-fast-queues-exp:enqueue i queue))
      (finish (cl-fast-queues-exp:queue-flush queue))
      (is eql t (cl-fast-queues-exp:queue-empty-p queue)))))

(define-test safe-fifo-exp-queue<->list :parent safe-fifo-exp
  (dotimes (i *loop-test-times*)
    (let ((items (loop for i below (+ 2 (random 50)) collect (random 10)))
          (queue1 (cl-fast-queues-exp:make-safe-fifo :init-length (+ 2 (random 50)))) ; :waitp nil))
          (queue2 nil))
      (dolist (item items)
        (cl-fast-queues-exp:enqueue item queue1))
      (is equal items (cl-fast-queues-exp:queue-to-list queue1))
      (setf queue2 (cl-fast-queues-exp:list-to-queue items :safe-fifo))
      (is equal items (cl-fast-queues-exp:queue-to-list queue2)))))


;;; ------- safe-lifo -------

(define-test make-safe-lifo-exp :parent safe-lifo-exp
  (finish (cl-fast-queues-exp:make-safe-lifo))
  (finish (cl-fast-queues-exp:make-safe-lifo :init-length 2))
  (fail (cl-fast-queues-exp:make-safe-lifo :init-length 1))
  (fail (cl-fast-queues-exp:make-safe-lifo :init-length 0)))

(define-test safe-lifo-exp-queue-count-0 :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue 0 queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-lifo-exp-queue-count-int :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue (random 10) queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-lifo-exp-queue-count-str :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue (write-to-string (random 10)) queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-lifo-exp-queue-count-nil :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues-exp:queue-count queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue nil queue))
      (is = count (cl-fast-queues-exp:queue-count queue)))))

(define-test safe-lifo-exp-queue-empty-p :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (cl-fast-queues-exp:enqueue nil queue)
        (is eq nil (cl-fast-queues-exp:queue-empty-p queue)))
      (dotimes (j count)
        (cl-fast-queues-exp:dequeue queue))
      (is eq t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (k (random 10))
        (cl-fast-queues-exp:dequeue queue)
        (is eq t (cl-fast-queues-exp:queue-empty-p queue))))))

(define-test safe-lifo-exp-enqueue :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (cl-fast-queues-exp:enqueue item queue)))))))

(define-test safe-lifo-exp-queue-peek :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (cl-fast-queues-exp:queue-peek queue) (eql nil) (eql nil))
      (cl-fast-queues-exp:enqueue first-item queue)
      (format t "enqueue finished.~%")
      (is equal first-item (cl-fast-queues-exp:queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (cl-fast-queues-exp:enqueue item queue)
          (is = item (cl-fast-queues-exp:queue-peek queue)))))))

(define-test safe-lifo-exp-dequeue :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           (items-rev (reverse items)))
      (is eq cl-speedy-lifo::*underflow-flag* (cl-fast-queues-exp:dequeue queue))
      (dolist (item items)
        (cl-fast-queues-exp:enqueue item queue))
      (dolist (item items-rev)
        (is = item (cl-fast-queues-exp:dequeue queue)))
      (is eq cl-speedy-lifo::*underflow-flag* (cl-fast-queues-exp:dequeue queue)))))

(define-test safe-lifo-exp-queue-find :parent safe-lifo-exp
  (dotimes (i *loop-test-times*)
    (let ((find-items (loop for i below (1+ (random 10)) collect (random 10)))
          (other-items (loop for i below (1+ (random 10)) collect (+ 100 (random 10))))
          (queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10))))) ; :waitp nil)))
      (dolist (item find-items) ; find in empty queue
        (false (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item (append other-items find-items)) ; enqueue, `find-items' euqueue last
        (cl-fast-queues-exp:enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item other-items) ; find queue
        (true (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item find-items) ; dequeue
        (cl-fast-queues-exp:dequeue queue))
      (dolist (item find-items) ; find dequeued items
        (false (cl-fast-queues-exp:queue-find item queue)))
      (dolist (item other-items) ; empty queue
        (cl-fast-queues-exp:dequeue queue))
      (dolist (item (nshuffle (append find-items other-items))) ; enqueue random list
        (cl-fast-queues-exp:enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (cl-fast-queues-exp:queue-find item queue))))))

(define-test safe-lifo-exp-queue-flush :parent safe-lifo-exp
  (dotimes (i *loop-test-times*)
    (let* ((count (random 20))
           (queue (cl-fast-queues-exp:make-safe-lifo :init-length (+ 2 (random 10)))))
      (finish (cl-fast-queues-exp:queue-flush queue))
      (dotimes (i count)
        (cl-fast-queues-exp:enqueue i queue))
      (finish (cl-fast-queues-exp:queue-flush queue))
      (is eql t (cl-fast-queues-exp:queue-empty-p queue))
      (dotimes (i count)
        (cl-fast-queues-exp:enqueue i queue))
      (finish (cl-fast-queues-exp:queue-flush queue))
      (is eql t (cl-fast-queues-exp:queue-empty-p queue)))))

(define-test safe-lifo-exp-queue<->list :parent safe-lifo-exp
  (dotimes (i *loop-test-times*)
    (let ((items (loop for i below (+ 2 (random 50)) collect (random 10)))
          (queue1 (cl-fast-queues-exp:make-safe-lifo :init-length (+  2 (random 50)))) ; :waitp nil))
          (queue2 nil))
      (dolist (item items)
        (cl-fast-queues-exp:enqueue item queue1))
      (is equal (reverse items) (cl-fast-queues-exp:queue-to-list queue1))
      (setf queue2 (cl-fast-queues-exp:list-to-queue items :safe-lifo))
      (is equal (reverse items) (cl-fast-queues-exp:queue-to-list queue2)))))












;;; ------- tests in multi-threads ------

;;; safe-fifo, threads test

(define-test safe-fifo-enqueue-threads :parent safe-fifo
  "enqueue in threads, dequeue in the main thread"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i 20000);*loop-test-times*)
    (let ((queue (make-safe-fifo :init-length 2));(1+ (random 10)))) ; :waitp t))
          (items (list 98 52 33)) ;(loop for i below (random 100) collect (random 100)))
          (enqueue-sum (make-atomic 0))
          (total 0))
      (format t "~&~%~%Loop times: ~d~%" i)
      (dolist (item items)
        (let ((it item))
          (bt:make-thread #'(lambda ()
                              (atomic-incf (atomic-place enqueue-sum) (enqueue it queue))))))
      (sleep 0.01) ; make sure all threads exited
      (unless (= (atomic-place enqueue-sum) (apply #'+ items))
        (sleep 1)
        (unless (= (atomic-place enqueue-sum) (apply #'+ items))
          (format t "~&A: expect total: ~d, real total: ~d, ~&items: ~d~&queue-to-list: ~d~%"
                  (apply #'+ items)
                  (atomic-place enqueue-sum)
                  items
                  (queue-to-list queue))))
      (is = (length items) (queue-count queue))
      (let ((queue-list (queue-to-list queue)))
        (unless (and (every #'integerp queue-list) (= (apply #'+ items) (apply #'+ queue-list)))
          (sleep 1)
          (format t "~&B: expect total: ~d, real total: ~d, ~&items: ~d~%queue-list: ~d~%raw queue: ~d~%"
                  (apply #'+ items)
                  (apply #'+ (queue-to-list queue))
                  items
                  (queue-to-list queue)
                  queue)
          (sleep 1)
          (format t "~&queue printing after a second sleep: ~d~%" queue)))
      (dotimes (j (length items))
        (setf total (+ total (dequeue queue))))
      (is = total (apply #'+ items))
      (true (queue-empty-p queue)))))

(define-test safe-fifo-dequeue-threads-no-wait :parent safe-fifo ; note: theres's no waitp arg anymore
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp t))
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

(define-test safe-fifo-dequeue-threads-wait :parent safe-fifo ; note: theres's no waitp arg anymore
  "enqueue in threads, then dequeue in threads"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i 1000);*loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (+ 2 (random 10)))) ; :waitp t))
           (items (loop for i below (random 100) collect (random 100)))
           (total (apply #'+ items)))
      (dolist (item items)
        (let ((it item))
          (bt:make-thread #'(lambda ()
                              (enqueue it queue)))))
      (sleep 0.1)
      (is = total
          (apply #'+
                 (loop for j below (length items)
                       collect (bt:join-thread
                                (bt:make-thread #'(lambda () (dequeue queue t)))))))
      (true (queue-empty-p queue)))))

(defparameter *send-to-push-list* nil)
(define-test safe-fifo-enqueue-threads2 :parent safe-fifo
  "test if all elements are enqueued correctly"
  (dotimes (i *loop-test-times*)
    (if (= 0 (mod (1+ i) 1000)) (format t "~&~%TIMES: ~D~%" (1+ I)))
    (sb-ext:gc :full t)
    (setf *send-to-push-list* nil)
    (let* ((queue (make-safe-fifo :init-length (+ 2 (random 10))))
           (items (loop for i below (random 100) collect (random 100)))
           (real-push-list nil))
      (dolist (item items)
        (let ((it item))
          (bt:make-thread #'(lambda ()
                              (sb-ext:atomic-push it *send-to-push-list*)
                              (enqueue it queue)))))
      (sleep 0.01)
      (setf real-push-list (queue-to-list queue))
      (unless (= (apply #'+ *send-to-push-list*) (apply #'+ real-push-list))
        (sleep 1)
        (is = (apply #'+ *send-to-push-list*) (apply #'+ (queue-to-list queue)))
        (unless (= (apply #'+ *send-to-push-list*) (apply #'+ (queue-to-list queue)))
          (format t "*send-to-push-list*: ~d~%queue list: ~d~%queue~d~%" *send-to-push-list* (queue-to-list queue) queue)))
      (is = (apply #'+ *send-to-push-list*) (apply #'+ real-push-list)))))

(defparameter *pop-list* nil)
(defparameter *problem-queue* nil)
(define-test safe-fifo-dequeue-threads :parent safe-fifo
  "make sure all items are correctly enqueued, then dequeue in threads, check if the dequeued items are those enqueued."
  (setf *problem-queue* nil)
  (dotimes (i 1000);*loop-test-times*)
    (if (= 0 (mod (1+ i) 1000)) (format t "~&~%TIMES: ~D~%" (1+ I)))
    (sb-ext:gc :full t)
    (setf *send-to-push-list* nil)
    (setf *pop-list* nil)
    (let* ((queue (make-safe-fifo :init-length 2));(+ 2 (random 10))))
           (items '(12 75 74 89 85 54 31 23 71 21 36 21 24 14 97 49 56 64 92 91 91 57 87 71 79 42
                    7 40 17 9 68 18 55 93 84 15 75 21 22 94 55 78 25 5 32 18 97 56 60 96 29 50 47
                    81 39 78 44 52 80 70 33 80 23 26 87 22 70 48 46 80 27 60 31 90 99 50 58 30 93
                    51 1 54 61 39 31 1 93 48));(loop for i below (random 100) collect (random 100)))
           (real-push-list nil)
           (dequeued+remainded nil)
           )
      (dolist (item items)
        (let ((it item))
          (bt:make-thread #'(lambda ()
                              (sb-ext:atomic-push it *send-to-push-list*)
                              (enqueue it queue)))))
      (sleep 0.01)
      (setf real-push-list (queue-to-list queue))
      (unless (= (apply #'+ *send-to-push-list*) (apply #'+ real-push-list)) ; wait and make sure all items are enqueued
        (sleep 1)
        (setf real-push-list (queue-to-list queue))
        (is = (apply #'+ *send-to-push-list*) (apply #'+ real-push-list))
        (unless (= (apply #'+ *send-to-push-list*) (apply #'+ (queue-to-list queue)))
          (format t "~&~%~%Push error!~%")
          (format t "~&*send-to-push-list*: ~d~%queue list: ~d~%queue: ~d~%" *send-to-push-list* (queue-to-list queue) queue)))

      (dolist (item items)
        (bt:make-thread #'(lambda () (sb-ext:atomic-push (dequeue queue t) *pop-list*))))
      (sleep 0.01)
      (setf dequeued+remainded (append (remove-if-not #'integerp *pop-list*)
                                       (queue-to-list queue)))
      (unless (= (apply #'+ dequeued+remainded) (apply #'+ *send-to-push-list*))
        (sleep 2)
        (setf dequeued+remainded (append (remove-if-not #'integerp *pop-list*)
                                         (queue-to-list queue)))
        ;;(is = (apply #'+ dequeued+remainded) (apply #'+ *send-to-push-list*))
        (unless (= (apply #'+ dequeued+remainded) (apply #'+ *send-to-push-list*))
          (format t "~&Pop error!~%")
          (format t "~&push-list: ~d~%pop-list: ~d~%remainder: ~d~%" *send-to-push-list* *pop-list* (queue-to-list queue))
          (format t "~&queue: ~d~%" queue)
          (push queue  *problem-queue*)))
      (true (queue-empty-p queue))
      (is = (apply #'+ dequeued+remainded) (apply #'+ *send-to-push-list*))
      )))



#+:ignore
(define-test safe-fifo-dequeue-threads-wait2 :parent safe-fifo ; note: theres's no waitp arg anymore
  "dequeue with waitp true in threads, then enqueue in threads"
  ;; cannot run, dequeue will return *under-flow*
  ;; ccl running failed
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)))) ; :waitp t))
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

#+:ignore
(define-test safe-fifo-dequeue-threads-wait3 :parent safe-fifo ;note: theres's no waitp arg anymore
  ;; cannot run, dequeue will return *under-flow*
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)))) ; :waitp t))
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

#+:ignore
(define-test safe-fifo-dequeue-threads-wait4 :parent safe-fifo
  "dequeue and atomic-incf in threads, enqueue in threads"
  ;; still failed after using bt's apiv2 if sole using (bt2:condition-wait cvar lock) in the def of dequeue,
  ;; but succeeded when I use a loop in the def of dequeue.
  ;; cannot run, dequeue will return *under-flow*
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((len (1+ (random 10)))
           (queue (make-safe-fifo :init-length len)) ; :waitp t))
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
    (let ((queue (make-safe-lifo :init-length (1+ (random 10)))) ; :waitp t))
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
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)))) ; :waitp nil))
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
    (let* ((queue (make-safe-fifo :init-length (1+ (random 10)))) ; :waitp t))
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

#+:ignore
(define-test safe-lifo-dequeue-threads-wait2-0 :parent safe-lifo
  "dequeue in threads with waitp true, then enqueue in threads"
  ;; stil failed in several cases
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)))) ; :waitp t))
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
#+:ignore
(define-test safe-lifo-dequeue-threads-wait2 :parent safe-lifo
  "dequeue with waitp true in threads, then enqueue in threads"
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 5)))) ; :waitp t))
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

#+:ignore
(define-test safe-lifo-dequeue-threads-wait3 :parent safe-lifo
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)))) ; :waitp t))
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

#+:ignore
(define-test safe-lifo-dequeue-threads-wait4 :parent safe-lifo
  "dequeue and atomic-incf in threads, enqueue in threads"
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-test-times*)
    (let* ((queue (make-safe-lifo :init-length (1+ (random 10)))) ; :waitp t))
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
