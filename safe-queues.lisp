(in-package :cl-fast-queues)


;;; safe unbound fifo queue

(defstruct (safe-fast-fifo (:conc-name safe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (underlay nil :type list) ; underlay can be implemented with any fifo structure
  (lock (bt:make-lock "SAFE-FIFO-LOCK")))

(defun safe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-fifo-p queue))

(defun make-safe-fifo (&key (init-length 1000)
                       &aux (queue (cl-speedy-queue-safe:make-queue init-length))
                         (underlay (%make-list-queue)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum init-length))
  (%list-queue-enqueue queue underlay)
  (make-safe-fast-fifo :underlay underlay
                       :push-queue queue
                       :pop-queue queue))

(defmethod queue-count ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (bt:with-lock-held (lock)
      (apply #'+ (mapcar #'cl-speedy-queue-safe:queue-count
                         (%list-queue-contents underlay))))))

(defmethod queue-empty-p ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pop-queue push-queue lock) queue
    (bt:with-lock-held (lock)
      (and (eq pop-queue push-queue)
           (cl-speedy-queue-safe:queue-empty-p pop-queue)))))

(defmethod enqueue (object (queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (let* ((push-queue (safe-fifo-push-queue queue))
           (res (cl-speedy-queue-safe:enqueue object push-queue)))
      (if (eq res #.*overflow-flag*)
          (bt:with-lock-held (lock)
            (let* ((retry-push-queue (safe-fifo-push-queue queue)) ; check if push-queue was changed by another thread
                   (retry-res (cl-speedy-queue-safe:enqueue object retry-push-queue)))
              (if (eq retry-res #.*overflow-flag*)
                  (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-queue-safe:queue-length retry-push-queue))
                                                           #.*enlarge-size*))))
                         (new-queue (cl-speedy-queue-safe:make-queue new-len)))
                    (cl-speedy-queue-safe:enqueue object new-queue)
                    (%list-queue-enqueue new-queue underlay)
                    (setf (safe-fifo-push-queue queue) new-queue)
                    object)
                  retry-res)))
          res))))

(defmethod queue-peek ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (let* ((pop-queue (safe-fifo-pop-queue queue))
           (res (multiple-value-list (cl-speedy-queue-safe:queue-peek pop-queue))))
      (if (second res)
          (values (first res) (second res))
          (bt:with-lock-held (lock)
            (let* ((retry-pop-queue (safe-fifo-pop-queue queue)) ; refetch to check if pop was changed by another thread
                   (retry-res (multiple-value-list (cl-speedy-queue-safe:queue-peek retry-pop-queue))))
              (if (and (null (second retry-res))
                       (cddr underlay)) ; cdr is the contents of the list-queue
                  (progn (%list-queue-dequeue underlay)
                         (setf pop-queue (%list-queue-peek underlay))
                         (cl-speedy-queue-safe:queue-peek pop-queue))
                  (values (first res) (second res)))))))))

(defmethod dequeue ((queue safe-fast-fifo) &optional keep-in-queue-p)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore keep-in-queue-p))
  (with-slots (underlay lock) queue
    (let* ((pop-queue (safe-fifo-pop-queue queue))
           (res (cl-speedy-queue-safe:dequeue pop-queue)))
      (if (eq res #.*underflow-flag*)
          (bt:with-lock-held (lock)
            (let* ((retry-pop-queue (safe-fifo-pop-queue queue)) ; refetch to check if pop was changed by another thread
                   (retry-res (cl-speedy-queue-safe:dequeue retry-pop-queue)))
              (if (and (eq retry-res #.*underflow-flag*)
                       (cddr underlay))
                  (progn (%list-queue-dequeue underlay)
                         (let* ((new-pop-queue (%list-queue-peek underlay))
                                (new-res (cl-speedy-queue-safe:dequeue new-pop-queue)))
                           (setf (safe-fifo-pop-queue queue) new-pop-queue)
                           new-res))
                  retry-res)))
          res))))

(defmethod queue-find (item (queue safe-fast-fifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (some #'(lambda (sfifo) (cl-speedy-queue-safe:queue-find item sfifo :key key :test test))
          (%list-queue-contents (safe-fifo-underlay queue)))))

(defmethod queue-flush ((queue safe-fast-fifo))
  "Empty the `queue'"
  (with-slots (push-queue pop-queue underlay lock) queue
    (bt:with-lock-held (lock)
      (setf push-queue (cl-speedy-queue-safe:queue-flush push-queue)
            pop-queue push-queue)
      (%list-queue-flush underlay)
      (%list-queue-enqueue push-queue underlay)
      queue)))

(defmethod queue-to-list ((queue safe-fast-fifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the same as enqueue order. (so that they will have the same dequeue order)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (mapcan #'cl-speedy-queue-safe:queue-to-list
            (%list-queue-contents (safe-fifo-underlay queue)))))

(defmethod list-to-queue (list (queue-type (eql :safe-fifo)))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-safe-fifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))


;;; ------- safe lifo unbound queue -------
;;;
;;; Cannot not use a LIFO queue as the underlay,
;;; because a LIFO underlay will make the current subqueue be the same structure, this may cause
;;; 1. frequent switching of creating and deleting a subqueue for the worst case;
;;; 2. pushing items into a popped subqueue.
;;; these problems can't be solved for such lock-underlay-atomic-subqueue idea.
;;;
;;; Instead, a doubly-linked list is utilized as the underlay, and the problems above solved.


(defstruct (safe-fast-lifo (:conc-name safe-lifo-))
  (cur-queue nil :type simple-vector)
  (underlay nil :type dlist:dlist)
  (lock (bt:make-lock "SAFE-LIFO-LOCK")))

(defun safe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-lifo-p queue))

(defun make-subqueue (init-length)
  (let ((cl-speedy-lifo-safe:*queue-start* 1))
    (declare (special cl-speedy-lifo-safe:*queue-start*))
    (let ((queue (cl-speedy-lifo-safe:make-queue init-length)))
      queue)))

(defun make-safe-lifo (&key (init-length 1000)
                       &aux (queue (make-subqueue init-length))
                         (underlay (dlist:make-dlist)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (dlist:insert-head underlay queue)
  (make-safe-fast-lifo :underlay underlay
                       :cur-queue queue))

(defmethod queue-count ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (apply #'+ (mapcar #'cl-speedy-lifo-safe:queue-count
                       (dlist:dlist-to-list (safe-lifo-underlay queue))))))

(defmethod queue-empty-p ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (bt:with-lock-held (lock)
      (and (dlist:dlist-single-p underlay)
           (cl-speedy-lifo-safe:queue-empty-p (dlist:node-content (dlist:dlist-head underlay)))))))

(defmethod enqueue (object (queue safe-fast-lifo)) ; enqueue should always be successful
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (let* ((cur-queue (safe-lifo-cur-queue queue))
           (res (cl-speedy-lifo-safe:enqueue object cur-queue)))
      (if (eq res #.*overflow-flag*)
          (bt:with-lock-held (lock)
            (let* ((retry-cur-queue (safe-lifo-cur-queue queue)) ; check if push-queue was changed by another thread
                   (retry-res (cl-speedy-lifo-safe:enqueue object retry-cur-queue)))
              (if (eq retry-res #.*overflow-flag*)
                  (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo-safe:queue-length retry-cur-queue))
                                                           #.*enlarge-size*))))
                         (new-queue (make-subqueue new-len)))
                    (dlist:insert-tail underlay new-queue)
                    (cl-speedy-lifo-safe:enqueue object
                                                 (setf (safe-lifo-cur-queue queue) new-queue)))
                  retry-res)))
          res))))

(defmethod queue-peek ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (let* ((cur-queue (safe-lifo-cur-queue queue))
           (res (multiple-value-list (cl-speedy-lifo-safe:queue-peek cur-queue))))
      (if (second res)
          (values (first res) (second res))
          (bt:with-lock-held (lock)
            (let ((head (dlist:dlist-head underlay))
                  (tail (dlist:dlist-tail underlay)))
              (if (eq head tail)
                  (values (first res) (second res))
                  (cl-speedy-lifo-safe:queue-peek (dlist:node-content (dlist:node-prev tail))))))))))

(defmethod dequeue ((queue safe-fast-lifo) &optional keep-in-queue-p)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore keep-in-queue-p))
  (with-slots (underlay lock) queue
    (let* ((cur-queue (safe-lifo-cur-queue queue))
           (res (cl-speedy-lifo-safe:dequeue cur-queue)))
      (if (eq res #.*underflow-flag*)
          (bt:with-lock-held (lock)
            (let ((head (dlist:dlist-head underlay))
                  (tail (dlist:dlist-tail underlay)))
              (if (eq head tail) ; if underlay has only one subqueue, return underflow
                  #.*underflow-flag*
                  (let* ((prev (dlist:node-prev tail))
                         (pop-queue (dlist:node-content prev))
                         (pop-item (cl-speedy-lifo-safe:dequeue pop-queue)))
                    (when (cl-speedy-lifo-safe:queue-empty-p pop-queue)
                      (dlist:remove-node underlay prev))
                    pop-item))))
          res))))

(defmethod queue-find (item (queue safe-fast-lifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (some #'(lambda (slifo) (cl-speedy-lifo-safe:queue-find item slifo :key key :test test))
          (dlist:dlist-elements (safe-lifo-underlay queue)))))

(defmethod queue-flush ((queue safe-fast-lifo))
  "Empty the `queue', do not use it when there some thread is doing dequeue/enqueue."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue lock) queue
    (bt:with-lock-held (lock)
      (setf (safe-lifo-cur-queue queue) (cl-speedy-lifo-safe:queue-flush cur-queue))
      (setf (slot-value queue 'underlay) (dlist:make-dlist))
      (dlist:insert-head (slot-value queue 'underlay) cur-queue)
      queue)))

(defmethod queue-to-list ((queue safe-fast-lifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the reverse of the enqueue order (so that they will have the same dequeue order)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (mapcan #'cl-speedy-lifo-safe:queue-to-list
            (reverse (dlist:dlist-elements (safe-lifo-underlay queue))))))

(defmethod list-to-queue (list (queue-type (eql :safe-lifo)))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-safe-lifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))
