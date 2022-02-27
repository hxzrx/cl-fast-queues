(in-package :cl-fast-queues)

;;; safe unbound fifo queue

(defstruct (safe-fast-fifo (:conc-name safe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (queue-list nil :type list)
  (lock (bt:make-lock "SAFE-FIFO-LOCK"))
  (cvar (bt:make-condition-variable :name "SAFE-FIFO-CVAR"))
  (enlarge-size 1.5 :type single-float))

(defun safe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-fifo-p queue))

(defun make-safe-fifo (&key (init-length 1000) (enlarge-size 1.5)
                       &aux (queue (cl-speedy-queue:make-queue init-length))
                         (queue-list (%make-list-queue)))
  (declare (fixnum init-length))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-safe-fast-fifo :queue-list (%list-queue-enqueue queue queue-list)
                       :push-queue queue
                       :pop-queue queue
                       :enlarge-size (float enlarge-size)))

(defmethod queue-count ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (apply #'+ (mapcar #'cl-speedy-queue:queue-count
                                 (%list-queue-contents (safe-fifo-queue-list queue))))))

(defmethod queue-empty-p ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-empty-p (safe-fifo-pop-queue queue)))

(defmethod enqueue (object (queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue queue-list enlarge-size push-queue-len lock cvar) queue
    (declare (single-float enlarge-size))
    (declare (fixnum push-queue-len))
    (bt:with-lock-held (lock)
      ;; when push-queue is full, add a new longer queue at the end of the list
      (when (cl-speedy-queue:queue-full-p push-queue)
        (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-queue:queue-length push-queue))
                                                 enlarge-size))))
               (new-queue (cl-speedy-queue:make-queue new-len)))
          (%list-queue-enqueue new-queue queue-list)
          (setf push-queue new-queue)))
      (prog1 (cl-speedy-queue:enqueue object push-queue)
        (bt:condition-notify cvar)))))

(defmethod queue-peek ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pop-queue) queue
    (cl-speedy-queue:queue-peek pop-queue)))

(defmethod dequeue ((queue safe-fast-fifo) &key (keep-in-queue-p t) (waitp nil))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if waitp
      (with-slots (pop-queue queue-list lock cvar) queue
        (bt:with-lock-held (lock)
          (prog1 (if (queue-empty-p queue)
                     ;; a loop was suggested in a bordeaux-threads GitHub issue conversation.
                     ;; https://github.com/sionescu/bordeaux-threads/issues/29
                     ;; but the tests shows that in sbcl the bug still exists when apiv2 was used without a loop.
                     (progn (loop while (queue-empty-p queue)
                                  do (progn (bt:thread-yield)
                                            (bt:condition-wait cvar lock)))
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


;;; safe lifo unbound queue

(defstruct (safe-fast-lifo (:conc-name safe-lifo-))
  (cur-queue nil :type simple-vector)
  (queue-list nil :type list)
  (lock (bt:make-lock "SAFE-LIFO-LOCK"))
  (cvar (bt:make-condition-variable :name "SAFE-LIFO-CVAR"))
  (enlarge-size 1.5 :type single-float))

(defun safe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-lifo-p queue))

(defun make-safe-lifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)
                       &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (<= enlarge-threshold 1.0))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-safe-fast-lifo :queue-list (list queue)
                       :cur-queue queue
                       :enlarge-size (float enlarge-size)))

(defmethod queue-count ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (apply #'+ (mapcar #'cl-speedy-lifo:queue-count
                     (safe-lifo-queue-list queue))))

(defmethod queue-empty-p ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-empty-p
   (car (safe-lifo-queue-list queue))))

(defmethod enqueue (object (queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list enlarge-size lock cvar) queue
    (declare (single-float enlarge-size))
    (bt:with-lock-held (lock)
      (when (cl-speedy-lifo:queue-full-p cur-queue)
        (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo:queue-length cur-queue))
                                                 enlarge-size))))
               (new-queue (cl-speedy-lifo:make-queue new-len)))
          (push new-queue queue-list)
          (setf cur-queue new-queue)))
      (prog1 (cl-speedy-lifo:enqueue object cur-queue)
        (bt:condition-notify cvar)))))

(defmethod queue-peek ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue) queue
    (cl-speedy-lifo:queue-peek cur-queue)))

(defmethod dequeue ((queue safe-fast-lifo) &key (keep-in-queue-p t) (waitp nil))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if waitp
      (with-slots (cur-queue queue-list cur-queue-len lock cvar) queue
        (declare (list queue-list))
        (bt:with-lock-held (lock)
          (prog1 (if (queue-empty-p queue)
                     ;; a loop was suggested in a bordeaux-threads GitHub issue conversation.
                     ;; https://github.com/sionescu/bordeaux-threads/issues/29
                     ;; but the tests shows that in sbcl the bug still exists when apiv2 was used without a loop.
                     (progn (loop while (queue-empty-p queue)
                                  do (progn (bt:thread-yield)
                                            (bt:condition-wait cvar lock)))
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
