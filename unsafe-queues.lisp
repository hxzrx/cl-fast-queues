(in-package :cl-fast-queues)

(declaim (inline %singularp))
(declaim (inline queue-empty-p))
(declaim (inline %unsafe-queue-empty-p))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *overflow-flag* #.cl-speedy-lifo:*overflow-flag*)
  (defvar *underflow-flag* #.cl-speedy-lifo:*underflow-flag*)
  (defvar *enlarge-size* 1.5))

(defun %singularp (lst)
  "Test if `lst' has only one element.
Note that it's not sufficient to test all singular list,
but it's enough in this lib since the car of lst will never be nil."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list lst))
  (and (eq nil (cdr lst))))

;;; fifo unbound queue

(defstruct (unsafe-fast-fifo (:conc-name unsafe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (queue-list nil :type list))

(defun unsafe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-fifo-p queue))

(defun make-unsafe-fifo (&key (init-length 1000)
                         &aux (queue (cl-speedy-queue:make-queue init-length))
                           (queue-list (%make-list-queue)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (make-unsafe-fast-fifo :queue-list (%list-queue-enqueue queue queue-list)
                         :push-queue queue
                         :pop-queue queue))

(defmethod queue-count ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (apply #'+ (mapcar #'cl-speedy-queue:queue-count
                                 (%list-queue-contents (unsafe-fifo-queue-list queue))))))

(defmethod queue-empty-p ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-empty-p (unsafe-fifo-pop-queue queue)))

#+:ignore
(defmethod enqueue (object (queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue queue-list) queue
    (declare (list queue-list))
    ;; when push-queue is full, add a new longer queue at the end of the list
    (when (cl-speedy-queue:queue-full-p push-queue)
      (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-queue:queue-length push-queue))
                                               #.*enlarge-size*))))
             (new-queue (cl-speedy-queue:make-queue new-len)))
        (%list-queue-enqueue new-queue queue-list)
        (setf push-queue new-queue)))
    (cl-speedy-queue:enqueue object push-queue)))

(defmethod enqueue (object (queue unsafe-fast-fifo)) ; faster
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue queue-list) queue
    (declare (list queue-list))
    (let ((res (cl-speedy-queue:enqueue object push-queue)))
      (if (eq res #.*overflow-flag*)
          (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-queue:queue-length push-queue))
                                                   #.*enlarge-size*))))
                 (new-queue (cl-speedy-queue:make-queue new-len)))
            (%list-queue-enqueue new-queue queue-list)
            (setf push-queue new-queue)
            (cl-speedy-queue:enqueue object push-queue))
          res))))

(defmethod queue-peek ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-peek (unsafe-fifo-pop-queue queue)))

#+:ignore
(defmethod dequeue ((queue unsafe-fast-fifo) &optional (keep-in-queue-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pop-queue queue-list) queue
    (prog1 (cl-speedy-queue:dequeue pop-queue keep-in-queue-p)
      (when (and (cl-speedy-queue:queue-empty-p pop-queue)
                 (null (%singularp (%list-queue-contents queue-list))))
        (%list-queue-dequeue queue-list)
        (setf pop-queue (%list-queue-peek queue-list))))))

(defmethod dequeue ((queue unsafe-fast-fifo) &optional (keep-in-queue-p t)) ; faster
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pop-queue queue-list) queue
    (let ((res (cl-speedy-queue:dequeue pop-queue keep-in-queue-p)))
      (if (and (eq res *underflow-flag*)
               (null (%singularp (%list-queue-contents queue-list))))
          (progn (%list-queue-dequeue queue-list)
                 (setf pop-queue (%list-queue-peek queue-list))
                 (cl-speedy-queue:dequeue pop-queue keep-in-queue-p))
          res))))

(defmethod queue-find (item (queue unsafe-fast-fifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (some #'(lambda (ufifo) (cl-speedy-queue:queue-find item ufifo :key key :test test))
        (%list-queue-contents (unsafe-fifo-queue-list queue))))

(defmethod queue-flush ((queue unsafe-fast-fifo))
  "Empty the `queue'"
  (with-slots (push-queue pop-queue queue-list) queue
    (setf push-queue (cl-speedy-queue:queue-flush push-queue)
          pop-queue push-queue)
    (%list-queue-flush queue-list)
    (%list-queue-enqueue push-queue queue-list)
    queue))

(defmethod queue-to-list ((queue unsafe-fast-fifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the same as queue order. (so that they will have the same dequeue order)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mapcan #'cl-speedy-queue:queue-to-list
          (%list-queue-contents (unsafe-fifo-queue-list queue))))

(defmethod list-to-queue (list (queue-type (eql :unsafe-fifo)) &optional (waitp t))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (declare (ignore waitp))
  (let* ((len (length list))
         (queue (make-unsafe-fifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))

;;; lifo unbound queue

(defstruct (unsafe-fast-lifo (:conc-name unsafe-lifo-))
  (cur-queue nil :type simple-vector)
  (queue-list nil :type list))

(defun unsafe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-lifo-p queue))

(defun make-unsafe-lifo (&key (init-length 1000)
                         &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (make-unsafe-fast-lifo :queue-list (list queue)
                         :cur-queue queue))

(defmethod queue-count ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (apply #'+ (mapcar #'cl-speedy-lifo:queue-count
                     (unsafe-lifo-queue-list queue))))

(defmethod queue-empty-p ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-empty-p
   (unsafe-lifo-cur-queue queue)))

#+:ignore
(defmethod enqueue (object (queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list) queue
    (declare (single-float *enlarge-size*))
    ;; when push-queue is full, push a new longer queue to queue-list
    (when (cl-speedy-lifo:queue-full-p cur-queue)
      (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo:queue-length cur-queue))
                                               *enlarge-size*))))
             (new-queue (cl-speedy-lifo:make-queue new-len)))
        (push new-queue queue-list)
        (setf cur-queue new-queue)))
    (cl-speedy-lifo:enqueue object cur-queue)))

(defmethod enqueue (object (queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list) queue
    ;; when push-queue is full, push a new longer queue to queue-list
    (let ((res (cl-speedy-lifo:enqueue object cur-queue)))
      (if (eq res #.*overflow-flag*)
          (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo:queue-length cur-queue))
                                                   #.*enlarge-size*))))
                 (new-queue (cl-speedy-lifo:make-queue new-len)))
            (push new-queue queue-list)
            (setf cur-queue new-queue)
            (cl-speedy-lifo:enqueue object cur-queue))
          res))))

(defmethod queue-peek ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-lifo:queue-peek (unsafe-lifo-cur-queue queue)))

#+:ignore
(defmethod dequeue ((queue unsafe-fast-lifo) &optional (keep-in-queue-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list) queue
    (declare (list queue-list))
    (prog1 (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p)
      (when (and (cl-speedy-lifo:queue-empty-p cur-queue)
                 (null (%singularp queue-list)))
        (setf queue-list (cdr queue-list)
              cur-queue (car  queue-list))))))

(defmethod dequeue ((queue unsafe-fast-lifo) &optional (keep-in-queue-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue queue-list) queue
    (format t "queue: ~d~%" queue)
    (let ((res (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p)))
      (if (and (eq res #.*underflow-flag*)
               (null (%singularp queue-list)))
          (progn (setf queue-list (cdr queue-list)
                       cur-queue  (car queue-list))
                 (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p))
          res))))

(defmethod queue-find (item (queue unsafe-fast-lifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (some #'(lambda (ulifo) (cl-speedy-lifo:queue-find item ulifo :key key :test test))
        (unsafe-lifo-queue-list queue)))

(defmethod queue-flush ((queue unsafe-fast-lifo))
  "Empty the `queue'"
  (with-slots (cur-queue queue-list) queue
    (setf cur-queue (cl-speedy-lifo:queue-flush cur-queue))
    (setf (cdr queue-list) nil
          (car queue-list) cur-queue)
    queue))

(defmethod queue-to-list ((queue unsafe-fast-lifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the reverse of the enqueue order (so that they will have the same dequeue order)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mapcan #'cl-speedy-lifo:queue-to-list (unsafe-lifo-queue-list queue)))

(defmethod list-to-queue (list (queue-type (eql :unsafe-lifo)) &optional (waitp t))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (declare (ignore waitp))
  (let* ((len (length list))
         (queue (make-unsafe-lifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))
