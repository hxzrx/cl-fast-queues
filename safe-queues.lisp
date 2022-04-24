(in-package :cl-fast-queues)


;;; safe unbound fifo queue

(defstruct (safe-fast-fifo (:conc-name safe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (queue-list nil :type list)
  (waitp t)
  (lock (bt:make-lock "SAFE-FIFO-LOCK"))
  (cvar (bt:make-condition-variable :name "SAFE-FIFO-CVAR")))

(defun safe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-fifo-p queue))

(defun make-safe-fifo (&key (init-length 1000) (waitp t)
                       &aux (queue (cl-speedy-queue:make-queue init-length))
                         (queue-list (%make-list-queue)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (make-safe-fast-fifo :queue-list (%list-queue-enqueue queue queue-list)
                       :push-queue queue
                       :pop-queue queue
                       :waitp waitp))

(defmethod queue-count ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (apply #'+ (mapcar #'cl-speedy-queue:queue-count
                       (%list-queue-contents (safe-fifo-queue-list queue))))))

(defmethod queue-empty-p ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (cl-speedy-queue:queue-empty-p (safe-fifo-pop-queue queue))))

(defmethod %unsafe-queue-empty-p ((queue safe-fast-fifo))
  "This method should only be used in a function's lock-held's body."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-empty-p (safe-fifo-pop-queue queue)))

(defmethod enqueue (object (queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue queue-list push-queue-len waitp lock cvar) queue
    (declare (fixnum push-queue-len))
    (bt:with-lock-held (lock)
      ;; when push-queue is full, add a new longer queue at the end of the list
      (when (cl-speedy-queue:queue-full-p push-queue)
        (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-queue:queue-length push-queue))
                                                 #.*enlarge-size*))))
               (new-queue (cl-speedy-queue:make-queue new-len)))
          (%list-queue-enqueue new-queue queue-list)
          (setf push-queue new-queue)))
      (prog1 (cl-speedy-queue:enqueue object push-queue)
        (when waitp
          (bt:condition-notify cvar))))))

(defmethod queue-peek ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (with-slots (pop-queue) queue
      (cl-speedy-queue:queue-peek pop-queue))))

(defmethod dequeue ((queue safe-fast-fifo) &optional (keep-in-queue-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (safe-fifo-waitp queue)
      (with-slots (pop-queue queue-list lock cvar) queue
        (bt:with-lock-held (lock)
          (prog1 (if (%unsafe-queue-empty-p queue)
                     (progn (loop while (%unsafe-queue-empty-p queue)
                                  do (bt:condition-wait cvar lock))
                            (cl-speedy-queue:dequeue pop-queue keep-in-queue-p))
                     (cl-speedy-queue:dequeue pop-queue keep-in-queue-p))
            (when (and (cl-speedy-queue:queue-empty-p pop-queue)
                       (null (%singularp (%list-queue-contents queue-list))))
              (%list-queue-dequeue queue-list)
              (setf pop-queue (%list-queue-peek queue-list))))))
      (with-slots (pop-queue queue-list lock) queue
        (bt:with-lock-held (lock)
          (prog1 (cl-speedy-queue:dequeue pop-queue keep-in-queue-p)
            (when (and (cl-speedy-queue:queue-empty-p pop-queue)
                       (null (%singularp (%list-queue-contents queue-list))))
              (%list-queue-dequeue queue-list)
              (setf pop-queue (%list-queue-peek queue-list))))))))

(defmethod queue-find (item (queue safe-fast-fifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (some #'(lambda (sfifo) (cl-speedy-queue:queue-find item sfifo :key key :test test))
          (%list-queue-contents (safe-fifo-queue-list queue)))))

(defmethod queue-flush ((queue safe-fast-fifo))
  "Empty the `queue'"
  (with-slots (push-queue pop-queue queue-list lock) queue
    (bt:with-lock-held (lock)
      (setf push-queue (cl-speedy-queue:queue-flush push-queue)
            pop-queue push-queue)
      (%list-queue-flush queue-list)
      (%list-queue-enqueue push-queue queue-list)
      queue)))

(defmethod queue-to-list ((queue safe-fast-fifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the same as enqueue order. (so that they will have the same dequeue order)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-fifo-lock queue))
    (mapcan #'cl-speedy-queue:queue-to-list
            (%list-queue-contents (safe-fifo-queue-list queue)))))

(defmethod list-to-queue (list (queue-type (eql :safe-fifo)) &optional (waitp t))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-safe-fifo :init-length len :waitp waitp)))
    (dolist (item list)
      (enqueue item queue))
    queue))


;;; safe lifo unbound queue

(defstruct (safe-fast-lifo (:conc-name safe-lifo-))
  (cur-queue nil :type simple-vector)
  (queue-list nil :type list)
  (waitp t)
  (lock (bt:make-lock "SAFE-LIFO-LOCK"))
  (cvar (bt:make-condition-variable :name "SAFE-LIFO-CVAR")))

(defun safe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-lifo-p queue))

(defun make-safe-lifo (&key (init-length 1000) (waitp t)
                       &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (make-safe-fast-lifo :queue-list (list queue)
                       :cur-queue queue
                       :waitp waitp))

(defmethod queue-count ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (apply #'+ (mapcar #'cl-speedy-lifo:queue-count
                       (safe-lifo-queue-list queue)))))

(defmethod queue-empty-p ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (cl-speedy-lifo:queue-empty-p
     (car (safe-lifo-queue-list queue)))))

(defmethod %unsafe-queue-empty-p ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-empty-p
   (car (safe-lifo-queue-list queue))))

(defmethod enqueue (object (queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list waitp lock cvar) queue
    (declare (single-float *enlarge-size*))
    (bt:with-lock-held (lock)
      (when (cl-speedy-lifo:queue-full-p cur-queue)
        (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo:queue-length cur-queue))
                                                 *enlarge-size*))))
               (new-queue (cl-speedy-lifo:make-queue new-len)))
          (push new-queue queue-list)
          (setf cur-queue new-queue)))
      (prog1 (cl-speedy-lifo:enqueue object cur-queue)
        (when waitp (bt:condition-notify cvar))))))

(defmethod queue-peek ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (with-slots (cur-queue) queue
      (cl-speedy-lifo:queue-peek cur-queue))))

(defmethod dequeue ((queue safe-fast-lifo) &optional (keep-in-queue-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (safe-lifo-waitp queue)
      (with-slots (cur-queue queue-list cur-queue-len lock cvar) queue
        (declare (list queue-list))
        (bt:with-lock-held (lock)
          (prog1 (if (%unsafe-queue-empty-p queue)
                     ;; a loop was suggested in a bordeaux-threads GitHub issue conversation.
                     ;; https://github.com/sionescu/bordeaux-threads/issues/29
                     ;; but the tests shows that in sbcl the bug still exists when apiv2 was used without a loop.
                     (progn (loop while (%unsafe-queue-empty-p queue)
                                  do (bt:condition-wait cvar lock))
                            (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p))
                     (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p))
            (when (and (cl-speedy-lifo:queue-empty-p cur-queue)
                       (null (%singularp queue-list)))
              (setf queue-list (cdr queue-list)
                    cur-queue (car  queue-list))))))
      (with-slots (cur-queue queue-list cur-queue-len lock) queue
        (declare (list queue-list))
        (bt:with-lock-held (lock)
          (prog1 (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p)
            (when (and (cl-speedy-lifo:queue-empty-p cur-queue)
                       (null (%singularp queue-list)))
              (setf queue-list (cdr queue-list)
                    cur-queue (car  queue-list))))))))

(defmethod queue-find (item (queue safe-fast-lifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (some #'(lambda (slifo) (cl-speedy-lifo:queue-find item slifo :key key :test test))
          (safe-lifo-queue-list queue))))

(defmethod queue-flush ((queue safe-fast-lifo))
  "Empty the `queue'"
  (with-slots (cur-queue queue-list lock) queue
    (bt:with-lock-held (lock)
      (setf cur-queue (cl-speedy-lifo:queue-flush cur-queue))
      (setf (cdr queue-list) nil
            (car queue-list) cur-queue)
      queue)))

(defmethod queue-to-list ((queue safe-fast-lifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the reverse of the enqueue order (so that they will have the same dequeue order)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (bt:with-lock-held ((safe-lifo-lock queue))
    (mapcan #'cl-speedy-lifo:queue-to-list (safe-lifo-queue-list queue))))

(defmethod list-to-queue (list (queue-type (eql :safe-lifo)) &optional (waitp t))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-safe-lifo :init-length len :waitp waitp)))
    (dolist (item list)
      (enqueue item queue))
    queue))
