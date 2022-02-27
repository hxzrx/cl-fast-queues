;;;; A queue implemented in Section 10.5 of the book "Paradigms of Artificial Intelligence Programming:
;;;;   Case Studies in Common Lisp" by Peter Norvig (1992)
;;;; https://github.com/norvig/paip-lisp/blob/main/docs/chapter10.md
;;;; The raw source code has been shared under MIT license.

(in-package :cl-fast-queues)

;;; A queue is a (last . contents) pair

(proclaim '(inline %list-queue-contents %make-list-queue %list-queue-enqueue %list-queue-dequeue
            %list-queue-peek %list-queue-empty-p %list-queue-nconc))

(defun %list-queue-contents (q) ; queue-contents in the raw src
  (cdr q))

(defun %make-list-queue () ; make-queue
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun %list-queue-enqueue (item q) ; enqueue
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun %list-queue-dequeue (q) ; dequeue
  "Remove an item from the front of the queue."
  ;; modified to return the popped content
  (let ((val (pop (cdr q))))
    (if (null (cdr q))
        (setf (car q) q))
    val))

(defun %list-queue-peek (q) ; front
  (first (%list-queue-contents q)))

(defun %list-queue-empty-p (q) ; empty-queue-p
  (null (%list-queue-contents q)))

(defun %list-queue-nconc (q list) ; queue-nconc
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))
