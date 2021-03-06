(in-package :cl-fast-queues-tests)

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

(define-test safe-fifo-queue-flush :parent safe-fifo
  (dotimes (i *loop-test-times*)
    (let* ((count (random 20))
           (queue (make-safe-fifo :init-length (1+ (random 10)))))
      (finish (cl-fast-queues::queue-flush queue))
      (dotimes (i count)
        (enqueue i queue))
      (finish (cl-fast-queues::queue-flush queue))
      (is eql t (queue-empty-p queue))
      (dotimes (i count)
        (enqueue i queue))
      (finish (cl-fast-queues::queue-flush queue))
      (is eql t (queue-empty-p queue)))))

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
