(in-package :cl-fast-queues)


;;; fifo unbound queue


(defvar *overflow-flag* cl-speedy-lifo:*overflow-flag*)
(defvar *underflow-flag* cl-speedy-lifo:*underflow-flag*)

(declaim (inline %singularp))
(defun %singularp (lst)
  "Test if `lst' has only one element.
Note that it's not sufficient to test all singular list,
but it's enough in this lib since the car of lst will never be nil."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list lst))
  (and (eq nil (cdr lst))))

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
  (queue-array (make-array 1 :initial-element nil :adjustable t)  :type vector)
  (cur-index 0 :type fixnum)
  (enlarge-size 1.5 :type single-float)
  (enlarge-threshold 1.0 :type single-float))

(defun %max-array-index-p (idx array)
  (= idx (1- (array-dimension array 0))))

(defun unsafe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-lifo-p queue))

(defun make-unsafe-lifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)
                         &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (<= enlarge-threshold 1.0))
  (assert (>= enlarge-size 1.0))
  (assert (> init-length 0))
  (make-unsafe-fast-lifo :queue-array (make-array 1 :initial-element queue :adjustable t)
                         :cur-queue queue
                         :enlarge-size (float enlarge-size)
                         :enlarge-threshold (float enlarge-threshold)))

(defmethod queue-count ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-index queue-array) queue
    (declare (fixnum cur-index))
    (declare (vector queue-array))
    (apply #'+ (map 'list #'cl-speedy-lifo:queue-count queue-array))))

(defmethod queue-empty-p ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-empty-p (unsafe-lifo-cur-queue queue)))

(defmethod enqueue (object (queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue cur-index queue-array enlarge-size enlarge-threshold) queue
    (declare (single-float enlarge-size enlarge-threshold))
    (declare (fixnum cur-index))
    (if (< (the fixnum (cl-speedy-lifo:queue-count cur-queue))
           (* enlarge-threshold (the fixnum (cl-speedy-lifo:queue-length cur-queue))))
        (cl-speedy-lifo:enqueue object cur-queue)
        (progn
          ;; enlarging queue-array and set it's last element to a larger cl-speedy-lifo
          (when (%max-array-index-p cur-index queue-array)
            (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo:queue-length cur-queue))
                                                     enlarge-size))))
                   (new-queue (cl-speedy-lifo:make-queue new-len)))
              (adjust-array queue-array (+ 2 cur-index)) ; cur-index = array-dimension -1
              (setf (aref queue-array (1+ cur-index)) new-queue)))
          (if (cl-speedy-lifo:queue-full-p cur-queue) ; check to switch to the next element of queue-array
              (progn (setf cur-queue (aref queue-array (1+ cur-index))
                           cur-index (1+ cur-index))
                     (cl-speedy-lifo:enqueue object cur-queue))
              (cl-speedy-lifo:enqueue object cur-queue))))))

(defmethod queue-peek ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue) queue
    (cl-speedy-lifo:queue-peek cur-queue)))

(defmethod dequeue ((queue unsafe-fast-lifo) &key (keep-in-queue-p t) waitp)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore waitp))
  (with-slots (cur-queue cur-index queue-array) queue
    (declare (fixnum cur-index))
    (prog1 (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p)
      ;; if cur-queue is empty, try to set it to the previous element
      (when (and (cl-speedy-lifo:queue-empty-p cur-queue)
                 (/= cur-index 0))
        (setf cur-index (1- cur-index)
              cur-queue (aref queue-array cur-index))))))
