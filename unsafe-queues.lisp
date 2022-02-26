(in-package :cl-fast-queues)


;;; fifo unbound queue


(defvar *overflow-flag* cl-speedy-lifo:*overflow-flag*)
(defvar *underflow-flag* cl-speedy-lifo:*underflow-flag*)

(defstruct (unsafe-fast-fifo (:conc-name unsafe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (queue-list nil :type list)
  (enlarge-size 1.5 :type single-float))

(defun unsafe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-fifo-p queue))

(defun make-unsafe-fifo (&key (init-length 1000) (enlarge-size 1.5)
                         &aux (queue (cl-speedy-queue:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-unsafe-fast-fifo :queue-list (list queue)
                         :push-queue queue
                         :pop-queue queue
                         :enlarge-size (float enlarge-size)))

(declaim (inline %singularp))
(defun %singularp (lst)
  "Test if `lst' has only one element.
Note that it's not sufficient to test all singular list,
but it's enough in this lib since the car of lst will never be nil."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list lst))
  (and (eq nil (cdr lst))))

(defmethod queue-count ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (apply #'+ (mapcar #'cl-speedy-queue:queue-count (unsafe-fifo-queue-list queue)))))

(defmethod queue-empty-p ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-empty-p (unsafe-fifo-pop-queue queue)))

(defmethod enqueue (object (queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue queue-list enlarge-size) queue
    (declare (single-float enlarge-size))
    (declare (list queue-list))
    ;; when push-queue is full, add a new longer queue at the end of the list
    (when (cl-speedy-queue:queue-full-p push-queue)
      (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-queue:queue-length push-queue))
                                               enlarge-size))))
             (new-queue (cl-speedy-queue:make-queue new-len)))
        (setf queue-list (nconc queue-list (list new-queue))
              push-queue new-queue)))
    (cl-speedy-queue:enqueue object push-queue)))

(defmethod queue-peek ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-peek (unsafe-fifo-pop-queue queue)))

(defmethod dequeue ((queue unsafe-fast-fifo) &key (keep-in-queue-p t) waitp)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore waitp))
  (with-slots (pop-queue queue-list) queue
    (prog1 (cl-speedy-queue:dequeue pop-queue keep-in-queue-p)
      (when (and (cl-speedy-queue:queue-empty-p pop-queue)
                 (null (%singularp queue-list)))
        (setf queue-list (cdr queue-list)
              pop-queue (car queue-list))))))


;;; lifo unbound queue

(defstruct (unsafe-fast-lifo (:conc-name unsafe-lifo-))
  (cur-queue nil :type simple-vector)
  (queue-list nil :type list)
  (enlarge-size 1.5 :type single-float))

(defun unsafe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-lifo-p queue))

(defun make-unsafe-lifo (&key (init-length 1000) (enlarge-size 1.5)
                         &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-unsafe-fast-lifo :queue-list (list queue)
                         :cur-queue queue
                         :enlarge-size (float enlarge-size)))

(defmethod queue-count ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (apply #'+ (mapcar #'cl-speedy-lifo:queue-count
                     (unsafe-lifo-queue-list queue))))

(defmethod queue-empty-p ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-empty-p
   (unsafe-lifo-cur-queue queue)))

(defmethod enqueue (object (queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list enlarge-size) queue
    (declare (single-float enlarge-size))
    ;; when push-queue is full, push a new longer queue to queue-list
    (when (cl-speedy-lifo:queue-full-p cur-queue)
      (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo:queue-length cur-queue))
                                               enlarge-size))))
             (new-queue (cl-speedy-lifo:make-queue new-len)))
        (push new-queue queue-list)
        (setf cur-queue new-queue)))
    (cl-speedy-lifo:enqueue object cur-queue)))

(defmethod queue-peek ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-peek (unsafe-lifo-cur-queue queue)))

(defmethod dequeue ((queue unsafe-fast-lifo) &key (keep-in-queue-p t) waitp)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore waitp))
  (with-slots (cur-queue queue-list) queue
    (declare (list queue-list))
    (prog1 (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p)
      (when (and (cl-speedy-lifo:queue-empty-p cur-queue)
                 (null (%singularp queue-list)))
        (setf queue-list (cdr queue-list)
              cur-queue (car  queue-list))))))
