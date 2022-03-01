;;;; A queue implemented in Section 10.5 of the book "Paradigms of Artificial Intelligence Programming:
;;;;   Case Studies in Common Lisp" by Peter Norvig (1992)
;;;; https://github.com/norvig/paip-lisp/blob/main/docs/chapter10.md
;;;; The raw source code has been shared under MIT license.

(in-package :cl-fast-queues)

;;; A queue is a (last . contents) pair
(proclaim '(inline %list-queue-contents %make-list-queue %list-queue-enqueue
            %list-queue-dequeue %list-queue-peek %list-queue-empty-p %list-queue-nconc))

(defun %list-queue-contents (q) ; queue-contents in the raw src
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cdr q))

(defun %make-list-queue () ; make-queue
  "Build a new queue, with no elements."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun %list-queue-enqueue (item q) ; enqueue
  "Insert item at the end of the queue."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun %list-queue-dequeue (q) ; dequeue
  "Remove an item from the front of the queue."
  ;; modified to return the popped content
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((val (pop (cdr q))))
    (if (null (cdr q))
        (setf (car q) q))
    val))

(defun %list-queue-peek (q) ; front
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (first (%list-queue-contents q)))

(defun %list-queue-empty-p (q) ; empty-queue-p
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (null (%list-queue-contents q)))

(defun %list-queue-nconc (q list) ; queue-nconc
  "Add the elements of LIST to the end of the queue."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setf (car q)
        (last (setf (rest (car q)) list))))
