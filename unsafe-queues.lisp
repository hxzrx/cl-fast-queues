(in-package :cl-fast-queues)


;;; fifo unbound queue


(defvar *overflow-flag* cl-speedy-lifo:*overflow-flag*)
(defvar *underflow-flag* cl-speedy-lifo:*underflow-flag*)

(defstruct (unsafe-fast-fifo (:conc-name unsafe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (queue-list nil :type list)
  (push-queue-len 1000 :type fixnum) ; length of push-queue
  (enlarge-size 1.5 :type single-float)
  (enlarge-threshold 1.0 :type single-float))

(defun unsafe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-fifo-p queue))

(defun make-unsafe-fifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)
                         &aux (queue (cl-speedy-queue:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (<= enlarge-threshold 1.0))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-unsafe-fast-fifo :queue-list (list queue)
                         :push-queue queue
                         :pop-queue queue
                         :push-queue-len (the fixnum init-length)
                         :enlarge-size (float enlarge-size)
                         :enlarge-threshold (float enlarge-threshold)))

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
  (with-slots (push-queue queue-list enlarge-size enlarge-threshold push-queue-len) queue
    (declare (single-float enlarge-size enlarge-threshold))
    (declare (fixnum push-queue-len))
    (if (< (the fixnum (cl-speedy-queue:queue-count push-queue))
           (* enlarge-threshold push-queue-len))
        (cl-speedy-queue:enqueue object push-queue)
        (progn
          (when ;;(%singularp queue-list) ; enlarging by add a new queue in the end of queue-list
              (eq push-queue (car (last queue-list)))
            (let* ((new-len (the fixnum (truncate (* push-queue-len enlarge-size))))
                   (new-queue (cl-speedy-queue:make-queue new-len)))
              (setf queue-list (nconc queue-list (list new-queue)))))
          (if (cl-speedy-queue:queue-full-p push-queue) ; check to switch to the last element of queue-list
              (progn (setf push-queue (car (last queue-list))
                           push-queue-len (the fixnum (cl-speedy-queue:queue-length push-queue)))
                     (cl-speedy-queue:enqueue object push-queue))
              (cl-speedy-queue:enqueue object push-queue))))))

(defmethod queue-peek ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pop-queue) queue
    (cl-speedy-queue:queue-peek pop-queue)))

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
  (cur-queue-len 1000 :type fixnum) ; length of cur-queue
  (enlarge-size 1.5 :type single-float)
  (enlarge-threshold 1.0 :type single-float))

(defun unsafe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-lifo-p queue))

(defun make-unsafe-lifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)
                         &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (<= enlarge-threshold 1.0))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-unsafe-fast-lifo :queue-list (list queue)
                         :cur-queue queue
                         :cur-queue-len init-length
                         :enlarge-size (float enlarge-size)
                         :enlarge-threshold (float enlarge-threshold)))

(defmethod queue-count ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (apply #'+ (mapcar #'cl-speedy-lifo:queue-count
                     (unsafe-lifo-queue-list queue))))

(defmethod queue-empty-p ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-empty-p
   (car (unsafe-lifo-queue-list queue))))

(defmethod enqueue (object (queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list enlarge-size enlarge-threshold cur-queue-len) queue
    (declare (single-float enlarge-size enlarge-threshold))
    (declare (fixnum cur-queue-len))
    (if (< (the fixnum (cl-speedy-lifo:queue-count cur-queue))
           (* enlarge-threshold cur-queue-len))
        (cl-speedy-lifo:enqueue object cur-queue)
        (progn
          (when ;;(%singularp queue-list) ; enlarging by add a new queue in the end of queue-list
              (eq cur-queue (car (last queue-list)))
            (let* ((new-len (the fixnum (truncate (* cur-queue-len enlarge-size))))
                   (new-queue (cl-speedy-lifo:make-queue new-len)))
              (setf queue-list (nconc queue-list (list new-queue)))))
          (if (cl-speedy-lifo:queue-full-p cur-queue) ; check to switch to the last element of queue-list
              (progn (setf cur-queue (car (last queue-list))
                           cur-queue-len (cl-speedy-lifo:queue-length cur-queue))
                     (cl-speedy-lifo:enqueue object cur-queue))
              (cl-speedy-lifo:enqueue object cur-queue))))))

(defmethod queue-peek ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue) queue
    (cl-speedy-lifo:queue-peek cur-queue)))

(defmethod dequeue ((queue unsafe-fast-lifo) &key (keep-in-queue-p t) waitp)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore waitp))
  (with-slots (cur-queue queue-list cur-queue-len) queue
    (declare (list queue-list))
    (prog1 (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p)
      (when (and (cl-speedy-lifo:queue-empty-p cur-queue)
                 (null (%singularp queue-list)))
        (setf queue-list (subseq queue-list 0 (1- (length queue-list)))
              cur-queue (car (last queue-list))
              cur-queue-len (cl-speedy-lifo:queue-length cur-queue))))))
