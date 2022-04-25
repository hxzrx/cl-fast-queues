(in-package :cl-fast-queues-tests)

;;; ------- safe-lifo -------

(define-test make-safe-lifo-exp :parent safe-lifo-exp
  (finish (cl-fast-queues:make-safe-lifo))
  (finish (cl-fast-queues:make-safe-lifo :init-length 2))
  (fail (cl-fast-queues:make-safe-lifo :init-length 1))
  (fail (cl-fast-queues:make-safe-lifo :init-length 0)))

(define-test safe-lifo-exp-queue-count-0 :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues:queue-count queue))
      (is eq t (cl-fast-queues:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues:enqueue 0 queue))
      (is = count (cl-fast-queues:queue-count queue)))))

(define-test safe-lifo-exp-queue-count-int :parent safe-fifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues:queue-count queue))
      (is eq t (cl-fast-queues:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues:enqueue (random 10) queue))
      (is = count (cl-fast-queues:queue-count queue)))))

(define-test safe-lifo-exp-queue-count-str :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues:queue-count queue))
      (is eq t (cl-fast-queues:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues:enqueue (write-to-string (random 10)) queue))
      (is = count (cl-fast-queues:queue-count queue)))))

(define-test safe-lifo-exp-queue-count-nil :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (is = 0 (cl-fast-queues:queue-count queue))
      (is eq t (cl-fast-queues:queue-empty-p queue))
      (dotimes (j count)
        (cl-fast-queues:enqueue nil queue))
      (is = count (cl-fast-queues:queue-count queue)))))

(define-test safe-lifo-exp-queue-empty-p :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (cl-fast-queues:enqueue nil queue)
        (is eq nil (cl-fast-queues:queue-empty-p queue)))
      (dotimes (j count)
        (cl-fast-queues:dequeue queue))
      (is eq t (cl-fast-queues:queue-empty-p queue))
      (dotimes (k (random 10))
        (cl-fast-queues:dequeue queue)
        (is eq t (cl-fast-queues:queue-empty-p queue))))))

(define-test safe-lifo-exp-enqueue :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100)))
      (dotimes (j count)
        (let ((item (random 10)))
          (is = item (cl-fast-queues:enqueue item queue)))))))

(define-test safe-lifo-exp-queue-peek :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
          (count (random 100))
          (first-item (elt '(0 nil "" :xxx 888) (random 5))))
      (is-values (cl-fast-queues:queue-peek queue) (eql nil) (eql nil))
      (cl-fast-queues:enqueue first-item queue)
      (format t "enqueue finished.~%")
      (is equal first-item (cl-fast-queues:queue-peek queue))
      (dotimes (j count)
        (let ((item (random 10)))
          (cl-fast-queues:enqueue item queue)
          (is = item (cl-fast-queues:queue-peek queue)))))))

(define-test safe-lifo-exp-dequeue :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
           (items (loop for i below (1+ (random 100)) collect (random 100)))
           (items-rev (reverse items)))
      (is eq cl-speedy-lifo::*underflow-flag* (cl-fast-queues:dequeue queue))
      (dolist (item items)
        (cl-fast-queues:enqueue item queue))
      (dolist (item items-rev)
        (is = item (cl-fast-queues:dequeue queue)))
      (is eq cl-speedy-lifo::*underflow-flag* (cl-fast-queues:dequeue queue)))))

(define-test safe-lifo-exp-queue-find :parent safe-lifo-exp
  (dotimes (i *loop-times*)
    (let ((find-items (loop for i below (1+ (random 10)) collect (random 10)))
          (other-items (loop for i below (1+ (random 10)) collect (+ 100 (random 10))))
          (queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10))))) ; :waitp nil)))
      (dolist (item find-items) ; find in empty queue
        (false (cl-fast-queues:queue-find item queue)))
      (dolist (item (append other-items find-items)) ; enqueue, `find-items' euqueue last
        (cl-fast-queues:enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (cl-fast-queues:queue-find item queue)))
      (dolist (item other-items) ; find queue
        (true (cl-fast-queues:queue-find item queue)))
      (dolist (item find-items) ; dequeue
        (cl-fast-queues:dequeue queue))
      (dolist (item find-items) ; find dequeued items
        (false (cl-fast-queues:queue-find item queue)))
      (dolist (item other-items) ; empty queue
        (cl-fast-queues:dequeue queue))
      (dolist (item (nshuffle (append find-items other-items))) ; enqueue random list
        (cl-fast-queues:enqueue item queue))
      (dolist (item find-items) ; find queue
        (true (cl-fast-queues:queue-find item queue))))))

(define-test safe-lifo-exp-queue-flush :parent safe-lifo-exp
  (dotimes (i *loop-times*)
    (let* ((count (random 20))
           (queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))))
      (finish (cl-fast-queues:queue-flush queue))
      (dotimes (i count)
        (cl-fast-queues:enqueue i queue))
      (finish (cl-fast-queues:queue-flush queue))
      (is eql t (cl-fast-queues:queue-empty-p queue))
      (dotimes (i count)
        (cl-fast-queues:enqueue i queue))
      (finish (cl-fast-queues:queue-flush queue))
      (is eql t (cl-fast-queues:queue-empty-p queue)))))

(define-test safe-lifo-exp-queue<->list :parent safe-lifo-exp
  (dotimes (i *loop-times*)
    (let ((items (loop for i below (+ 2 (random 50)) collect (random 10)))
          (queue1 (cl-fast-queues:make-safe-lifo :init-length (+  2 (random 50)))) ; :waitp nil))
          (queue2 nil))
      (dolist (item items)
        (cl-fast-queues:enqueue item queue1))
      (is equal (reverse items) (cl-fast-queues:queue-to-list queue1))
      (setf queue2 (cl-fast-queues:list-to-queue items :safe-lifo))
      (is equal (reverse items) (cl-fast-queues:queue-to-list queue2)))))


;;; ------- tests in multi-threads ------

;;; safe-lifo, threads test

(define-test safe-lifo-exp-enqueue-threads :parent safe-lifo-exp
  "enqueue in threads, dequeue in the main thread"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10))))
          (items (loop for i below (random 100) collect (random 100)))
          (threads nil)
          (total 0))
      (dolist (item items)
        (let ((it item))
          (push
           (bt:make-thread #'(lambda ()
                               (cl-fast-queues:enqueue it queue)))
           threads)))
      (dolist (th threads) (bt:join-thread th))
      (is = (length items) (cl-fast-queues:queue-count queue))
      (dotimes (j (length items))
        (setf total (+ total (cl-fast-queues:dequeue queue))))
      (is = total (apply #'+ items))
      (true (cl-fast-queues:queue-empty-p queue)))))

(define-test safe-lifo-exp-dequeue-threads-no-wait :parent safe-lifo-exp
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10)))) ; :waitp nil))
           (items (loop for i below (random 100) collect (random 100)))
           (push-threads nil)
           (total (apply #'+ items)))
      (dolist (item items)
        (let ((it item))
          (push
           (bt:make-thread #'(lambda ()
                               (cl-fast-queues:enqueue it queue)))
           push-threads)))
      (dolist (th push-threads) (bt:join-thread th))
      (is = total
          (apply #'+
                 (loop for j below (length items)
                       collect (bt:join-thread
                                (bt:make-thread #'(lambda () (cl-fast-queues:dequeue queue t)))))))
      (true (cl-fast-queues:queue-empty-p queue)))))

(define-test safe-lifo-exp-dequeue-threads-wait :parent safe-lifo-exp
  "enqueue in threads, then dequeue in threads with waitp true"
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((queue (cl-fast-queues:make-safe-fifo :init-length (+ 2 (random 10))))
           (items (loop for i below (random 100) collect (random 100)))
           (threads nil)
           (total (apply #'+ items)))
      (dolist (item items)
        (let ((it item))
          (push
           (bt:make-thread #'(lambda ()
                               (cl-fast-queues:enqueue it queue)))
           threads)))
      (dolist (th threads) (bt:join-thread th))
      (is = total
          (apply #'+
                 (loop for j below (length items)
                       collect (bt:join-thread
                                (bt:make-thread #'(lambda () (cl-fast-queues:dequeue queue t)))))))
      (true (cl-fast-queues:queue-empty-p queue)))))

#+sbcl
(define-test safe-lifo-exp-dequeue/enqueue-threads :parent safe-lifo-exp
  "dequeue in threads, enqueue in threads, check if the sum of the dequeued items and the ones still in the queue are equal."
  #+sbcl (sb-ext:gc :full t)
  (dotimes (i *loop-times*)
    (let* ((num (+ 2 (random 5)))
           (queue (cl-fast-queues:make-safe-lifo :init-length num)) ; :waitp t))
           (items (loop for i below (random 20) collect (random 10)))
           (push-threads nil)
           (pop-threads nil)
           (dequeued+remainded nil)
           (items-sum (apply #'+ items)))
      (setf *dequeue-list* (list))
      (setf *enqueue-list* (list))
      (dolist (item items)
        (push
         (bt:make-thread #'(lambda ()
                             (sb-ext:atomic-push (cl-fast-queues:dequeue queue nil)
                                                 *dequeue-list*)))
         pop-threads))
      (dolist (item items)
        (let ((it item))
          (push
           (bt:make-thread #'(lambda ()
                            (sb-ext:atomic-push (cl-fast-queues:enqueue it queue)
                                                *enqueue-list*)))
           push-threads)))
      (dolist (th pop-threads) (bt:join-thread th))
      (dolist (th push-threads) (bt:join-thread th))
      (setf dequeued+remainded (append (remove-if-not #'integerp *dequeue-list*)
                                       (cl-fast-queues:queue-to-list queue)))
      (is = items-sum (apply #'+ *enqueue-list*))
      (is = items-sum (apply #'+ dequeued+remainded))
      (unless (= items-sum (apply #'+ dequeued+remainded))
        (format t "~&init-nul: ~d~&items: ~d~&dequeue-list: ~d~&remainder: ~d~%"
                num
                items
                *dequeue-list*
                (cl-fast-queues:queue-to-list queue)))
      )))

#+sbcl
(define-test safe-lifo-exp-enqueue/dequeue-threads :parent safe-lifo-exp
  "dequeue in threads, enqueue in threads, check if the sum of the dequeued items and the ones still in the queue are equal."
  (dotimes (i *loop-times*)
    (sb-ext:gc :full t)
    (setf *send-to-push-list* nil)
    (setf *pop-list* nil)
    (let* ((queue (cl-fast-queues:make-safe-lifo :init-length (+ 2 (random 10))))
           (items (loop for i below (random 100) collect (random 100)))
           (push-threads nil)
           (pop-threads nil)
           (dequeued+remainded nil))
      (dolist (item items)
        (let ((it item))
          (push
           (bt:make-thread #'(lambda ()
                               (sb-ext:atomic-push it *send-to-push-list*)
                               (cl-fast-queues:enqueue it queue)))
           push-threads)))
      (dolist (item items)
        (push
         (bt:make-thread #'(lambda () (sb-ext:atomic-push (cl-fast-queues:dequeue queue t) *pop-list*)))
         pop-threads))

      (dolist (th pop-threads) (bt:join-thread th))
      (dolist (th push-threads) (bt:join-thread th))

      (setf dequeued+remainded (append (remove-if-not #'integerp *pop-list*)
                                       (cl-fast-queues:queue-to-list queue)))
      (is = (apply #'+ dequeued+remainded) (apply #'+ *send-to-push-list*)))))

#+ccl
(define-test safe-lifo-exp-dequeue/enqueue-threads :parent safe-lifo-exp
  "dequeue in threads, enqueue in threads, check if the sum of the dequeued items and the ones still in the queue are equal."
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((num (+ 2 (random 5)))
           (queue (cl-fast-queues:make-safe-lifo :init-length num)) ; :waitp t))
           (items (loop for i below (random 20) collect (random 10)))
           (push-threads nil)
           (pop-threads nil)
           (items-sum (apply #'+ items)))
      (setf *dequeue-sum* (make-atomic 0))
      (setf *enqueue-sum* (make-atomic 0))
      (dolist (item items)
        (declare (ignore item))
        (push (bt:make-thread #'(lambda ()
                                  (let ((res (cl-fast-queues:dequeue queue nil)))
                                    (if (integerp res)
                                        (atomic-incf (atomic-place *dequeue-sum*) res)))))
              pop-threads))
      (dolist (item items)
        (let ((it item))
          (push (bt:make-thread #'(lambda ()
                                    (let ((res (cl-fast-queues:enqueue it queue)))
                                      (if (integerp res)
                                          (atomic-incf (atomic-place *enqueue-sum*) res)))))
                push-threads)))
      (dolist (th pop-threads) (bt:join-thread th))
      (dolist (th push-threads) (bt:join-thread th))
      (is = items-sum (atomic-place *enqueue-sum*))
      (is = items-sum (+ (atomic-place *dequeue-sum*) (apply #'+ (cl-fast-queues:queue-to-list queue))))
      )))

#+ccl
(define-test safe-lifo-exp-enqueue/dequeue-threads :parent safe-lifo-exp
  "dequeue in threads, enqueue in threads, check if the sum of the dequeued items and the ones still in the queue are equal."
  #+ccl (ccl:gc)
  (dotimes (i *loop-times*)
    (let* ((num (+ 2 (random 5)))
           (queue (cl-fast-queues:make-safe-lifo :init-length num)) ; :waitp t))
           (items (loop for i below (random 20) collect (random 10)))
           (push-threads nil)
           (pop-threads nil)
           (items-sum (apply #'+ items)))
      (setf *dequeue-sum* (make-atomic 0))
      (setf *enqueue-sum* (make-atomic 0))
      (dolist (item items)
        (let ((it item))
          (push (bt:make-thread #'(lambda ()
                                    (let ((res (cl-fast-queues:enqueue it queue)))
                                      (if (integerp res)
                                          (atomic-incf (atomic-place *enqueue-sum*) res)))))
                push-threads)))
      (dolist (item items)
        (declare (ignore item))
        (push (bt:make-thread #'(lambda ()
                                  (let ((res (cl-fast-queues:dequeue queue nil)))
                                    (if (integerp res)
                                        (atomic-incf (atomic-place *dequeue-sum*) res)))))
              pop-threads))
      (dolist (th push-threads) (bt:join-thread th))
      (dolist (th pop-threads) (bt:join-thread th))
      (is = items-sum (atomic-place *enqueue-sum*))
      (is = items-sum (+ (atomic-place *dequeue-sum*) (apply #'+ (cl-fast-queues:queue-to-list queue))))
      )))
