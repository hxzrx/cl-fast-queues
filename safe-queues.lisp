(in-package :cl-fast-queues)


;;; safe unbound fifo queue

(defstruct (safe-fast-fifo (:conc-name safe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (underlay nil :type simple-vector) ; underlay can be implemented with any fifo structure
  (lock (bt:make-lock "SAFE-FIFO-LOCK")))

(defun safe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-fifo-p queue))

(defun make-safe-fifo (&key (init-length 1000)
                       &aux (queue (cl-speedy-queue-safe:make-queue init-length))
                         (underlay (cl-speedy-queue-safe:make-queue 100)))
  (declare (fixnum init-length))
  (cl-speedy-queue-safe:enqueue queue underlay)
  (make-safe-fast-fifo :underlay underlay
                       :push-queue queue
                       :pop-queue queue))

(defmethod queue-count ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (bt:with-lock-held (lock)
      (apply #'+ (mapcar #'cl-speedy-queue-safe:queue-count
                         (cl-speedy-queue-safe:queue-to-list underlay))))))

(defmethod queue-empty-p ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pop-queue push-queue lock) queue
    (bt:with-lock-held (lock)
      (and (eq pop-queue push-queue)
           (cl-speedy-queue-safe:queue-empty-p pop-queue)))))

(defmethod enqueue (object (queue safe-fast-fifo)) ; faster
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
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
                    (cl-speedy-queue-safe:enqueue new-queue underlay)
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
                       (> (the fixnum (cl-speedy-queue-safe:queue-count underlay)) 1))
                  (progn (cl-speedy-queue-safe:dequeue underlay)
                         (setf pop-queue (cl-speedy-queue-safe:queue-peek underlay))
                         (cl-speedy-queue-safe:queue-peek pop-queue))
                  (values (first res) (second res)))))))))

(defmethod dequeue ((queue safe-fast-fifo) &optional keep-in-queue-p)
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore keep-in-queue-p))
  (with-slots (underlay lock) queue
    (let* ((pop-queue (safe-fifo-pop-queue queue))
           (res (cl-speedy-queue-safe:dequeue pop-queue)))
      (if (eq res *underflow-flag*)
          (bt:with-lock-held (lock)
            (let* ((retry-pop-queue (safe-fifo-pop-queue queue)) ; refetch to check if pop was changed by another thread
                   (retry-res (cl-speedy-queue-safe:dequeue retry-pop-queue)))
              (if (and (eq retry-res *underflow-flag*)
                       (> (the fixnum (cl-speedy-queue-safe:queue-count underlay)) 1))
                  (progn (cl-speedy-queue-safe:dequeue underlay)
                         (let* ((new-pop-queue (cl-speedy-queue-safe:queue-peek underlay))
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
          (cl-speedy-queue-safe:queue-to-list (safe-fifo-underlay queue)))))

(defmethod queue-flush ((queue safe-fast-fifo))
  "Empty the `queue'"
  (with-slots (push-queue pop-queue underlay lock) queue
    (bt:with-lock-held (lock)
      (setf push-queue (cl-speedy-queue-safe:queue-flush push-queue)
            pop-queue push-queue)
      (cl-speedy-queue-safe:queue-flush underlay)
      (cl-speedy-queue-safe:enqueue push-queue underlay)
      queue)))

(defmethod queue-to-list ((queue safe-fast-fifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the same as enqueue order. (so that they will have the same dequeue order)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (mapcan #'cl-speedy-queue-safe:queue-to-list
            (cl-speedy-queue-safe:queue-to-list (safe-fifo-underlay queue)))))

(defmethod list-to-queue (list (queue-type (eql :safe-fifo)))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-safe-fifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))


;;; safe lifo unbound queue

(defstruct (safe-fast-lifo (:conc-name safe-lifo-))
  (cur-queue nil :type simple-vector)
  (underlay nil :type simple-vector)
  ;;(queue-list nil :type list)
  (lock (bt:make-lock "SAFE-LIFO-LOCK"))
  (cvar (bt:make-condition-variable :name "SAFE-LIFO-CVAR")))

(defun safe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-lifo-p queue))

(defun make-safe-lifo (&key (init-length 1000)
                       &aux (queue (cl-speedy-lifo-safe:make-queue init-length))
                         (underlay (cl-speedy-lifo-safe:make-queue 100)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (cl-speedy-lifo-safe:enqueue queue underlay)
  (make-safe-fast-lifo :underlay underlay
                       :cur-queue queue))

(defmethod queue-count ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (apply #'+ (mapcar #'cl-speedy-lifo-safe:queue-count
                       (cl-speedy-lifo-safe:queue-to-list (safe-lifo-underlay queue))))))

(defmethod queue-empty-p ((queue safe-fast-lifo))
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay lock) queue
    (bt:with-lock-held (lock)
      (and (= (cl-speedy-lifo-safe:queue-count underlay) 1)
           (cl-speedy-lifo-safe:queue-empty-p (cl-speedy-lifo-safe:queue-peek underlay))))))

(defmethod enqueue (object (queue safe-fast-lifo))
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
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
                         (new-queue (cl-speedy-lifo-safe:make-queue new-len)))
                    (cl-speedy-lifo-safe:enqueue new-queue underlay)
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
            (let* ((retry-cur-queue (safe-lifo-cur-queue queue)) ; refetch to check if pop was changed by another thread
                   (retry-res (multiple-value-list (cl-speedy-lifo-safe:queue-peek retry-cur-queue))))
              (if (and (null (second retry-res))
                       (> (the fixnum (cl-speedy-lifo-safe:queue-count underlay)) 1))
                  (progn (cl-speedy-lifo-safe:dequeue underlay)
                         (setf pop-queue (cl-speedy-lifo-safe:queue-peek underlay))
                         (cl-speedy-lifo-safe:queue-peek cur-queue))
                  (values (first res) (second res)))))))))

(defmethod dequeue ((queue safe-fast-lifo) &optional keep-in-queue-p)
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore keep-in-queue-p))
  (with-slots (underlay lock) queue
    (let* ((cur-queue (safe-lifo-cur-queue queue))
           (res (cl-speedy-lifo-safe:dequeue cur-queue)))
      (if (eq res *underflow-flag*)
          (bt:with-lock-held (lock)
            (let* ((retry-cur-queue (safe-lifo-cur-queue queue)) ; refetch to check if pop was changed by another thread
                   (retry-res (cl-speedy-lifo-safe:dequeue retry-cur-queue)))
              (if (and (eq retry-res *underflow-flag*)
                       (> (the fixnum (cl-speedy-lifo-safe:queue-count underlay)) 1))
                  (progn (cl-speedy-lifo-safe:dequeue underlay)
                         (cl-speedy-lifo-safe:dequeue (setf (safe-lifo-cur-queue queue)
                                                            (cl-speedy-lifo-safe:queue-peek underlay))))
                  retry-res)))
          res))))

(defmethod queue-find (item (queue safe-fast-lifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (some #'(lambda (slifo) (cl-speedy-lifo-safe:queue-find item slifo :key key :test test))
          (cl-speedy-lifo-safe:queue-to-list (safe-lifo-underlay queue)))))

(defmethod queue-flush ((queue safe-fast-lifo))
  "Empty the `queue', do not use it when there some thread is doing dequeue/enqueue."
  (with-slots (cur-queue underlay lock) queue
    (bt:with-lock-held (lock)
      (setf (safe-lifo-cur-queue queue) (cl-speedy-lifo-safe:queue-flush cur-queue))
      (cl-speedy-lifo-safe:queue-flush underlay)
      (cl-speedy-lifo-safe:enqueue cur-queue underlay)
      queue)))

(defmethod queue-to-list ((queue safe-fast-lifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the reverse of the enqueue order (so that they will have the same dequeue order)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (mapcan #'cl-speedy-lifo-safe:queue-to-list
            (cl-speedy-lifo-safe:queue-to-list (safe-lifo-underlay queue)))))

(defmethod list-to-queue (list (queue-type (eql :safe-lifo)))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-safe-lifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))
