(in-package :cl-fast-queues)

(declaim (inline %singularp %remove-second))
(declaim (inline unsafe-fifo-push-queue unsafe-fifo-pop-queue unsafe-fifo-underlay))
(declaim (inline unsafe-lifo-cur-queue unsafe-lifo-underlay))
(declaim (inline queue-count queue-empty-p enqueue dequeue queue-peek queue-find queue-flush))

(defun %singularp (lst)
  "Test if `lst' has only one element."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list lst))
  (and lst (eq nil (cdr lst))))

(defun %remove-second (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list lst))
  (setf (cdr lst) (cddr lst))
  lst)

;;; fifo unbound queue

(defstruct (unsafe-fast-fifo (:conc-name unsafe-fifo-))
  (push-queue nil :type simple-vector)
  (pop-queue  nil :type simple-vector)
  (underlay   nil :type list))

(defun unsafe-fifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-fifo-p queue))

(defun make-unsafe-fifo (&key (init-length 1000)
                         &aux (queue (cl-speedy-queue:make-queue init-length))
                           (underlay (%make-list-queue)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (make-unsafe-fast-fifo :underlay (%list-queue-enqueue queue underlay)
                         :push-queue queue
                         :pop-queue queue))

(defmethod queue-count ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (apply #'+ (mapcar #'cl-speedy-queue:queue-count
                                 (%list-queue-contents (unsafe-fifo-underlay queue))))))

(defmethod queue-empty-p ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-empty-p (unsafe-fifo-pop-queue queue)))

(defmethod enqueue (object (queue unsafe-fast-fifo)) ; faster
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (push-queue underlay) queue
    (declare (list underlay))
    (let ((res (cl-speedy-queue:enqueue object push-queue)))
      (if (eq res #.*overflow-flag*)
          (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-queue:queue-length push-queue))
                                                   #.*enlarge-size*))))
                 (new-queue (cl-speedy-queue:make-queue new-len)))
            (%list-queue-enqueue new-queue underlay)
            (setf push-queue new-queue)
            (cl-speedy-queue:enqueue object push-queue))
          res))))

(defmethod queue-peek ((queue unsafe-fast-fifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-speedy-queue:queue-peek (unsafe-fifo-pop-queue queue)))

(defmethod dequeue ((queue unsafe-fast-fifo) &optional (keep-in-queue-p t)) ; faster
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (pop-queue underlay) queue
    (let ((res (cl-speedy-queue:dequeue pop-queue keep-in-queue-p)))
      (if (and (eq res *underflow-flag*)
               (null (%singularp (%list-queue-contents underlay))))
          (progn (%list-queue-dequeue underlay)
                 (setf pop-queue (%list-queue-peek underlay))
                 (cl-speedy-queue:dequeue pop-queue keep-in-queue-p))
          res))))

(defmethod queue-find (item (queue unsafe-fast-fifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (some #'(lambda (ufifo) (cl-speedy-queue:queue-find item ufifo :key key :test test))
        (%list-queue-contents (unsafe-fifo-underlay queue))))

(defmethod queue-flush ((queue unsafe-fast-fifo))
  "Empty the `queue'"
  (with-slots (push-queue pop-queue underlay) queue
    (setf push-queue (cl-speedy-queue:queue-flush push-queue)
          pop-queue push-queue)
    (%list-queue-flush underlay)
    (%list-queue-enqueue push-queue underlay)
    queue))

(defmethod queue-to-list ((queue unsafe-fast-fifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the same as queue order. (so that they will have the same dequeue order)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mapcan #'cl-speedy-queue:queue-to-list
          (%list-queue-contents (unsafe-fifo-underlay queue))))

(defmethod list-to-queue (list (queue-type (eql :unsafe-fifo)))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-unsafe-fifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))

;;; lifo unbound queue

(defstruct (unsafe-fast-lifo (:conc-name unsafe-lifo-))
  (cur-queue nil :type simple-vector)
  (underlay  nil :type list))

(defun unsafe-lifo-p (queue)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unsafe-fast-lifo-p queue))

(defun make-unsafe-lifo (&key (init-length 1000)
                         &aux (queue (cl-speedy-lifo:make-queue init-length)))
  (declare (fixnum init-length))
  (assert (> init-length 0))
  (make-unsafe-fast-lifo :underlay (list queue)
                         :cur-queue queue))

(defmethod queue-count ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (apply #'+ (mapcar #'cl-speedy-lifo:queue-count
                     (unsafe-lifo-underlay queue))))

(defmethod queue-empty-p ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay) queue
    (and (%singularp underlay)
         (cl-speedy-lifo:queue-empty-p (car underlay)))))

(defmethod enqueue (object (queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue underlay) queue
    ;; when push-queue is full, push a new longer queue to underlay
    (let ((res (cl-speedy-lifo:enqueue object cur-queue)))
      (if (eq res #.*overflow-flag*)
          (let* ((new-len (the fixnum (truncate (* (the fixnum (cl-speedy-lifo:queue-length cur-queue))
                                                   #.*enlarge-size*))))
                 (new-queue (cl-speedy-lifo:make-queue new-len))
                 (ret (cl-speedy-lifo:enqueue object new-queue)))
            (push new-queue underlay)
            (setf (unsafe-lifo-cur-queue queue) new-queue)
            ret)
          res))))

(defmethod queue-peek ((queue unsafe-fast-lifo))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay) queue
    (let* ((cur-queue (unsafe-lifo-cur-queue queue))
           (res (multiple-value-list (cl-speedy-lifo:queue-peek cur-queue))))
      (if (second res)
          (values (first res) (second res))
          (alexandria:if-let (2nd (second underlay))
            (cl-speedy-lifo:queue-peek 2nd)
            (values (first res) (second res)))))))

(defmethod dequeue ((queue unsafe-fast-lifo) &optional (keep-in-queue-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (underlay) queue
    (let* ((cur-queue (unsafe-lifo-cur-queue queue))
           (res (cl-speedy-lifo:dequeue cur-queue keep-in-queue-p)))
      (if (eq res #.*underflow-flag*)
          (alexandria:if-let (2nd (second underlay))
            (let* ((pop-item (cl-speedy-lifo:dequeue 2nd keep-in-queue-p)))
              (when (cl-speedy-lifo:queue-empty-p 2nd)
                (%remove-second underlay))
              pop-item)
            #.*underflow-flag*)
          res))))

(defmethod queue-find (item (queue unsafe-fast-lifo) &key (key #'identity) (test #'eql))
  "If `item' has been found in `queue', return the item that has been found, or else return nil.
So if `item' is nil, the returned value will be nil whatever."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (some #'(lambda (ulifo) (cl-speedy-lifo:queue-find item ulifo :key key :test test))
        (unsafe-lifo-underlay queue)))

(defmethod queue-flush ((queue unsafe-fast-lifo))
  "Empty the `queue'"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (cur-queue) queue
    (cl-speedy-lifo:queue-flush cur-queue)
    (setf (slot-value queue 'underlay) (list cur-queue))
    queue))

(defmethod queue-to-list ((queue unsafe-fast-lifo))
  "Return a list of items those have been enqueued,
and the order of the returned list is the reverse of the enqueue order (so that they will have the same dequeue order)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mapcan #'cl-speedy-lifo:queue-to-list
          (unsafe-lifo-underlay queue)))

(defmethod list-to-queue (list (queue-type (eql :unsafe-lifo)))
  "Make a queue, then enque the items in the list from left to right."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (list list))
  (let* ((len (length list))
         (queue (make-unsafe-lifo :init-length len)))
    (dolist (item list)
      (enqueue item queue))
    queue))
