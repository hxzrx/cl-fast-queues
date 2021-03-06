(in-package :cl-fast-queues-tests)


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

(define-test speedy-queue-basic :parent speedy-queue
  (let* ((len 20)
         (queue (cl-speedy-queue:make-queue len))
         (lst (loop for i from 0 below len collect i)))
    (is = 0 (cl-speedy-queue:queue-count queue))
    (is = len (cl-speedy-queue:queue-length queue))
    (is-values (cl-speedy-queue:queue-peek queue) (eql nil) (eql nil))
    (false (cl-speedy-queue:queue-full-p queue))
    (true (cl-speedy-queue:queue-empty-p queue))
    (is equal nil (cl-speedy-queue:queue-to-list queue))
    (is eql nil (cl-speedy-queue:queue-find (1+ len) queue))
    (is eql nil (cl-speedy-queue:queue-find nil queue))
    ;;(is eql nil (cl-speedy-queue:queue-find cl-speedy-queue::queue-sentinel queue))
    (loop for item in lst
          for idx from 0
          for count from 1
          do (progn (finish (cl-speedy-queue:enqueue item queue))
                    (is = count (cl-speedy-queue:queue-count queue))
                    (is = len (cl-speedy-queue:queue-length queue))
                    (is-values (cl-speedy-queue:queue-peek queue) (eql (first lst)) (eql t))
                    (if (= count len)
                        (true (cl-speedy-queue:queue-full-p queue))
                        (false (cl-speedy-queue:queue-full-p queue)))
                    (false (cl-speedy-queue:queue-empty-p queue))
                    (is equal (subseq lst 0 count) (cl-speedy-queue:queue-to-list queue))
                    (is eql nil (cl-speedy-queue:queue-find (1+ len) queue))
                    (is eql nil (cl-speedy-queue:queue-find nil queue))
                    ;;(is eql nil (cl-speedy-queue:queue-find cl-speedy-queue::queue-sentinel queue))
                    (loop for element in (subseq lst 0 count)
                          do (progn (is = element (cl-speedy-queue:queue-find element queue))))
                    (loop for element in (subseq lst count len)
                          do (progn (is eql nil (cl-speedy-queue:queue-find element queue))))
                    ))
    (loop for idx from 0 below len
          for count from 1
          do (progn (finish (cl-speedy-queue:dequeue queue))
                    (is = (- len count) (cl-speedy-queue:queue-count queue))
                    (is = len (cl-speedy-queue:queue-length queue))
                    (if (= count len)
                        (is-values (cl-speedy-queue:queue-peek queue) (eql nil) (eql nil))
                        (is-values (cl-speedy-queue:queue-peek queue) (eql (nth count lst)) (eql t)))
                    (false (cl-speedy-queue:queue-full-p queue))
                    (if (= count len)
                        (true (cl-speedy-queue:queue-empty-p queue))
                        (false (cl-speedy-queue:queue-empty-p queue)))
                    (is equal (subseq lst count len) (cl-speedy-queue:queue-to-list queue))
                    (is eql nil (cl-speedy-queue:queue-find (1+ len) queue))
                    (is eql nil (cl-speedy-queue:queue-find nil queue))
                    ;;(is eql nil (cl-speedy-queue:queue-find cl-speedy-queue::queue-sentinel queue))
                    (loop for element in (subseq lst count len)
                          do (progn (is = element (cl-speedy-queue:queue-find element queue))))
                    (loop for element in (subseq lst 0 count)
                          do (progn (is eql nil (cl-speedy-queue:queue-find element queue))))
      ))
    ))
