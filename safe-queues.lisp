(in-package :cl-fast-queues)

;;; safe unbound fifo queue

(defstruct (safe-fast-fifo (:conc-name safe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (queue-list nil :type list)
  (lock (bt:make-lock "SAFE-FIFO-LOCK"))
  (cvar (bt:make-condition-variable :name "SAFE-FIFO-CVAR"))
  (push-queue-len 1000 :type fixnum) ; length of push-queue
  (enlarge-size 1.5 :type single-float)
  (enlarge-threshold 1.0 :type single-float))

(defun safe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (safe-fast-fifo-p queue))

(defun make-safe-fifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)
                       &aux (queue (cl-speedy-queue:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (<= enlarge-threshold 1.0))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-safe-fast-fifo :queue-list (list queue)
                       :push-queue queue
                       :pop-queue queue
                       :push-queue-len (the fixnum init-length)
                       :enlarge-size (float enlarge-size)
                       :enlarge-threshold (float enlarge-threshold)))

(defmethod queue-count ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (apply #'+ (mapcar #'cl-speedy-queue:queue-count (safe-fifo-queue-list queue))))

(defmethod queue-empty-p ((queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-empty-p (safe-fifo-pop-queue queue)))

(defmethod enqueue (object (queue safe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue queue-list enlarge-size enlarge-threshold push-queue-len lock cvar) queue
    (declare (single-float enlarge-size enlarge-threshold))
    (declare (fixnum push-queue-len))
    (bt:with-lock-held (lock)
      (if (< (the fixnum (cl-speedy-queue:queue-count push-queue))
             (* enlarge-threshold push-queue-len))
          (prog1 (cl-speedy-queue:enqueue object push-queue)
            (bt:condition-notify cvar))
          (progn
            (when ;;(%singularp queue-list) ; enlarging by add a new queue in the end of queue-list
                (eq push-queue (car (last queue-list)))
              (let* ((new-len (the fixnum (truncate (* push-queue-len enlarge-size))))
                     (new-queue (cl-speedy-queue:make-queue new-len)))
                (setf queue-list (nconc queue-list (list new-queue)))))
            ;; in this condition, the queue will never be empty, so we needn't notify the cvar
            (if (cl-speedy-queue:queue-full-p push-queue) ; check to switch to the last element of queue-list
                (progn (setf push-queue (car (last queue-list))
                             push-queue-len (cl-speedy-queue:queue-length push-queue))
                       (prog1 (cl-speedy-queue:enqueue object push-queue)
                         (bt:condition-notify cvar)))
                (prog1 (cl-speedy-queue:enqueue object push-queue)
                  (bt:condition-notify cvar))))))))

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
                     ;; a loop was suggested in a bordeaux-threads GitHub issue conversation. but it doesn't work on CCL.
                     ;; https://github.com/sionescu/bordeaux-threads/issues/29
                     (progn (bt:condition-wait cvar lock)
                            (cl-speedy-queue:dequeue pop-queue keep-in-queue-p))
                     (cl-speedy-queue:dequeue pop-queue keep-in-queue-p))
            (when (and (cl-speedy-queue:queue-empty-p pop-queue)
                       (null (%singularp queue-list)))
              (setf queue-list (cdr queue-list)
                    pop-queue (car queue-list))))))
      (with-slots (pop-queue queue-list lock) queue
        (bt:with-lock-held (lock)
          (prog1 (cl-speedy-queue:dequeue pop-queue keep-in-queue-p)
            (when (and (cl-speedy-queue:queue-empty-p pop-queue)
                       (null (%singularp queue-list)))
              (setf queue-list (cdr queue-list)
                    pop-queue (car queue-list))))))))


;;; safe lifo unbound queue

(defstruct (safe-fast-lifo (:conc-name safe-lifo-))
  (push-queue nil :type simple-vector)
  (queue-list nil :type list)
  (lock (bt:make-lock "SAFE-LIFO-LOCK"))
  (cvar (bt:make-condition-variable :name "SAFE-LIFO-CVAR"))
  (push-queue-len 1000 :type fixnum) ; length of push-queue
  (enlarge-size 1.5 :type single-float)
  (enlarge-threshold 1.0 :type single-float))

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
                       :push-queue queue
                       :push-queue-len init-length
                       :enlarge-size (float enlarge-size)
                       :enlarge-threshold (float enlarge-threshold)))

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
  (with-slots (push-queue queue-list enlarge-size enlarge-threshold push-queue-len lock cvar) queue
    (declare (single-float enlarge-size enlarge-threshold))
    (declare (fixnum push-queue-len))
    (bt:with-lock-held (lock)
      (if (< (the fixnum (cl-speedy-lifo:queue-count push-queue))
             (* enlarge-threshold push-queue-len))
          (prog1 (cl-speedy-lifo:enqueue object push-queue)
            (bt:condition-notify cvar))
          (progn
            (when ;;(%singularp queue-list) ; enlarging by add a new queue in the end of queue-list
                (eq push-queue (car (last queue-list)))
              (let* ((new-len (the fixnum (truncate (* push-queue-len enlarge-size))))
                     (new-queue (cl-speedy-lifo:make-queue new-len)))
                (setf queue-list (nconc queue-list (list new-queue)))))
            (if (cl-speedy-lifo:queue-full-p push-queue) ; check to switch to the last element of queue-list
                (progn (setf push-queue (car (last queue-list))
                             push-queue-len (cl-speedy-lifo:queue-length push-queue))
                       (prog1 (cl-speedy-lifo:enqueue object push-queue)
                         (bt:condition-notify cvar)))
                (prog1 (cl-speedy-lifo:enqueue object push-queue)
                  (bt:condition-notify cvar)
                  )))))))

(defmethod queue-peek ((queue safe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue) queue
    (cl-speedy-lifo:queue-peek push-queue)))

(defmethod dequeue ((queue safe-fast-lifo) &key (keep-in-queue-p t) (waitp nil))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if waitp
      (with-slots (push-queue queue-list lock cvar) queue
        (declare (list queue-list))
        (bt:with-lock-held (lock)
          (prog1 (if (queue-empty-p queue)
                     ;; a loop was suggested in a bordeaux-threads GitHub issue conversation. but it doesn't work on CCL.
                     ;; https://github.com/sionescu/bordeaux-threads/issues/29
                     (progn (bt:condition-wait cvar lock)
                            (cl-speedy-lifo:dequeue push-queue keep-in-queue-p))
                     (cl-speedy-lifo:dequeue push-queue keep-in-queue-p)))
          (when (and (cl-speedy-lifo:queue-empty-p push-queue)
                     (null (%singularp queue-list)))
            (setf queue-list (subseq queue-list 0 (1- (length queue-list)))
                  push-queue (car (last queue-list))))))
      (with-slots (push-queue queue-list lock) queue
        (declare (list queue-list))
        (bt:with-lock-held (lock)
          (prog1 (cl-speedy-lifo:dequeue push-queue keep-in-queue-p)
            (when (and (cl-speedy-lifo:queue-empty-p push-queue)
                       (null (%singularp queue-list)))
              (setf queue-list (subseq queue-list 0 (1- (length queue-list)))
                    push-queue (car (last queue-list)))))))))
