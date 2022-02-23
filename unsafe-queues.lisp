(in-package :cl-fast-queues)


;;; fifo unbound queue


(defvar *overflow-flag* cl-speedy-lifo:*overflow-flag*)
(defvar *underflow-flag* cl-speedy-lifo:*underflow-flag*)

(defstruct (unsafe-fast-fifo (:conc-name unsafe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (queue-list nil :type list)
  (push-queue-len 1000 :type fixnum) ; length of push-queue
  (enlarge-size 1.5 :type float)
  (enlarge-threshold 1.0 :type float))

(defun unsafe-fifo-p (queue)
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
                         :push-queue-len init-length
                         :enlarge-size (float enlarge-size)
                         :enlarge-threshold (float enlarge-threshold)))

(defun %singularp (lst)
  "Test if `lst' has only one element.
Note that it's not sufficient to test all singular list,
but it's enough in this lib since the car of lst will never be nil."
  (and (eq nil (cdr lst))))

(defmethod queue-count ((queue unsafe-fast-fifo))
  (apply #'+ (mapcar #'cl-speedy-queue:queue-count (unsafe-fifo-queue-list queue))))

(defmethod queue-empty-p ((queue unsafe-fast-fifo))
  (cl-speedy-queue:queue-empty-p (unsafe-fifo-pop-queue queue)))

(defmethod enqueue (object (queue unsafe-fast-fifo))
  (with-slots (push-queue queue-list enlarge-size enlarge-threshold push-queue-len) queue
    (if (< (cl-speedy-queue:queue-count push-queue)
           (* enlarge-threshold push-queue-len))
        (cl-speedy-queue:enqueue object push-queue)
        (progn
          (when ;;(%singularp queue-list) ; enlarging by add a new queue in the end of queue-list
              (eq push-queue (car (last queue-list)))
            (let* ((new-len (the fixnum (floor (* push-queue-len enlarge-size))))
                   (new-queue (cl-speedy-queue:make-queue new-len)))
              (setf queue-list (nconc queue-list (list new-queue)))))
          (if (cl-speedy-queue:queue-full-p push-queue) ; check to switch to the last element of queue-list
              (progn (setf push-queue (car (last queue-list))
                           push-queue-len (cl-speedy-queue:queue-length push-queue))
                     (cl-speedy-queue:enqueue object push-queue))
              (cl-speedy-queue:enqueue object push-queue))))))

(defmethod queue-peek ((queue unsafe-fast-fifo))
  (with-slots (pop-queue) queue
    (cl-speedy-queue:queue-peek pop-queue)))

(defmethod dequeue ((queue unsafe-fast-fifo) &optional (keep-in-queue-p t) waitp)
  (declare (ignore waitp))
  (with-slots (pop-queue queue-list) queue
    (prog1 (cl-speedy-queue:dequeue pop-queue keep-in-queue-p)
      (when (and (cl-speedy-queue:queue-empty-p pop-queue)
                 (null (%singularp queue-list)))
        (setf queue-list (cdr queue-list)
              pop-queue (car queue-list))))))


;;; lifo unbound queue

(defstruct (unsafe-fast-lifo (:conc-name unsafe-lifo-))
  (push-queue nil :type simple-vector)
  (queue-list nil :type list)
  (push-queue-len 1000 :type fixnum) ; length of push-queue
  (enlarge-size 1.5 :type float)
  (enlarge-threshold 1.0 :type float))

(defun unsafe-lifo-p (queue)
  (unsafe-fast-lifo-p queue))

(defun make-unsafe-lifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)
                         &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (<= enlarge-threshold 1.0))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-unsafe-fast-lifo :queue-list (list queue)
                         :push-queue queue
                         :push-queue-len init-length
                         :enlarge-size (float enlarge-size)
                         :enlarge-threshold (float enlarge-threshold)))

(defmethod queue-count ((queue unsafe-fast-lifo))
  (apply #'+ (mapcar #'cl-speedy-lifo:queue-count
                     (unsafe-lifo-queue-list queue))))

(defmethod queue-empty-p ((queue unsafe-fast-lifo))
  (cl-speedy-lifo:queue-empty-p
   (car (unsafe-lifo-queue-list queue))))

(defmethod enqueue (object (queue unsafe-fast-lifo))
  (with-slots (push-queue queue-list enlarge-size enlarge-threshold push-queue-len) queue
    (if (< (cl-speedy-lifo:queue-count push-queue)
           (* enlarge-threshold push-queue-len))
        (cl-speedy-lifo:enqueue object push-queue)
        (progn
          (when ;;(%singularp queue-list) ; enlarging by add a new queue in the end of queue-list
              (eq push-queue (car (last queue-list)))
            (let* ((new-len (the fixnum (floor (* push-queue-len enlarge-size))))
                   (new-queue (cl-speedy-lifo:make-queue new-len)))
              (setf queue-list (nconc queue-list (list new-queue)))))
          (if (cl-speedy-lifo:queue-full-p push-queue) ; check to switch to the last element of queue-list
              (progn (setf push-queue (car (last queue-list))
                           push-queue-len (cl-speedy-lifo:queue-length push-queue))
                     (cl-speedy-lifo:enqueue object push-queue))
              (cl-speedy-lifo:enqueue object push-queue))))))

(defmethod queue-peek ((queue unsafe-fast-lifo))
  (with-slots (push-queue) queue
    (cl-speedy-lifo:queue-peek push-queue)))

(defmethod dequeue ((queue unsafe-fast-lifo) &optional (keep-in-queue-p t) waitp)
  (declare (ignore waitp))
  (with-slots (push-queue queue-list) queue
    (prog1 (cl-speedy-lifo:dequeue push-queue keep-in-queue-p)
      (when (and (cl-speedy-lifo:queue-empty-p push-queue)
                 (null (%singularp queue-list)))
        (setf queue-list (subseq queue-list 0 (1- (length queue-list)))
              push-queue (car (last queue-list)))))))
