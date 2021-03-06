(in-package :cl-fast-queues-tests)


(define-test speedy-queue-flush :parent speedy-queue-safe
  (fail (cl-speedy-queue-safe:make-queue 0))
  (fail (cl-speedy-queue-safe:make-queue 1))
  (dotimes (i *loop-test-times*)
    (let* ((count (+ 2 (random 10)))
           (queue (cl-speedy-queue-safe:make-queue count))
           (most-enq (random count)))
      (dotimes (i most-enq)
        (cl-speedy-queue-safe:enqueue 0 queue))
      (finish (cl-speedy-queue-safe:queue-flush queue))
      (is eq t (cl-speedy-queue-safe:queue-empty-p queue))
      (dotimes (i most-enq)
        (cl-speedy-queue-safe:enqueue 0 queue))
      (finish (cl-speedy-queue-safe:queue-flush queue))
      (is eq t (cl-speedy-queue-safe:queue-empty-p queue)))))

(define-test speedy-queue-basic :parent speedy-queue-safe
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
                    ;;(is eql nil (cl-speedy-queue-safe:queue-find cl-speedy-queue-safe::queue-sentinel queue))
                    (loop for element in (subseq lst count len)
                          do (progn (is = element (cl-speedy-queue-safe:queue-find element queue))))
                    (loop for element in (subseq lst 0 count)
                          do (progn (is eql nil (cl-speedy-queue-safe:queue-find element queue))))
      ))
    ))

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
                    ;;(is eql nil (cl-speedy-queue-safe:queue-find cl-speedy-queue-safe::queue-sentinel queue))
                    (loop for element in (subseq lst count len)
                          do (progn (is = element (cl-speedy-queue-safe:queue-find element queue))))
                    (loop for element in (subseq lst 0 count)
                          do (progn (is eql nil (cl-speedy-queue-safe:queue-find element queue))))
      ))))

(define-test speedy-queue-safe-dequeue-enqueue-mixed-threads :parent speedy-queue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i 10000);*loop-test-times*)
    (when (mod (1+ i) 1000) #+sbcl (sb-ext:gc :full t) #+ccl (ccl:gc))
    (setf *enqueue-sum* (make-atomic 0))
    (setf *dequeue-sum* (make-atomic 0))
    (let* ((n (+ 2 (random 20))) ; queue length
           (queue (cl-speedy-queue-safe:make-queue n))
           (push-threads nil)
           (pop-threads nil)
           (k (random (1+ n))) ; randomly fill [0, n] itmes
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

(defparameter *one-thread-push-num* (expt 10 5))
(defparameter *thread-num-list* (list 1 2 3 4 5 6 7 8))

(define-test speedy-queue-safe-dequeue-enqueue-mixed-threads-2 :parent speedy-queue-safe
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  (dotimes (i 10) ;*loop-test-times*)
    (format t "~%~%safe speedy queue test, times: ~d" i)
    (when (mod (1+ i) 1000) #+sbcl (sb-ext:gc :full t) #+ccl (ccl:gc))
    (dolist (thread-num *thread-num-list*)
      (format t "~&thread num: ~d~%" thread-num)
      (setf *enqueue-sum* (make-atomic 0))
      (setf *dequeue-sum* (make-atomic 0))
      (let* ((n *one-thread-push-num*)
             (queue (cl-speedy-queue-safe:make-queue (* n thread-num)))
             (item 1)
             (push-threads nil)
             (pop-threads nil)
             (total (* item n thread-num)))
        (dotimes (th thread-num)
          (push (bt:make-thread #'(lambda ()
                                    (dotimes (push-times n)
                                      (let ((res (cl-speedy-queue-safe:enqueue item queue)))
                                        (if (integerp res)
                                            (atomic-incf (atomic-place *enqueue-sum*) res)
                                            (format t "~&To enqueue: ~d, ret: ~d, queue: ~d~%" item res queue))))))
                push-threads))
        (dotimes (th thread-num)
          (push (bt:make-thread #'(lambda ()
                                    (dotimes (push-times n)
                                      (let ((res (cl-speedy-queue-safe:dequeue queue t)))
                                        (if (integerp res)
                                            (atomic-incf (atomic-place *dequeue-sum*) res)
                                            #+:ignore(format t "~&Dequeue return: ~d, queue: ~d~%" res queue))))))
                pop-threads))

        (dolist (thread push-threads)
          (bt:join-thread thread))
        (dolist (thread pop-threads)
          (bt:join-thread thread))

        (is = total (atomic-place *enqueue-sum*))
        (is = total (+ (atomic-place *dequeue-sum*)
                       (apply #'+ (cl-speedy-queue-safe:queue-to-list queue))))

        #+:ignore(unless (cl-speedy-queue-safe:queue-empty-p queue)
          (format t "~&queue not empty: ~d~%elements: ~d~%" queue (cl-speedy-queue-safe:queue-to-list queue)))
        #+:ignore(unless (= total (+ (atomic-place *dequeue-sum*)
                            (apply #'+ (cl-speedy-queue-safe:queue-to-list queue))))
          (format t "~&items num: ~d, enqueue sum: ~d, expect sum: ~d~%" (/ total n) (atomic-place *enqueue-sum*) total)
          (format t "~&Queue count: ~d~%" (cl-speedy-queue-safe:queue-count queue)))
        ))))
