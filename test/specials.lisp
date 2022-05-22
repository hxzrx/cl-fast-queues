(in-package :cl-fast-queues-tests)

(defparameter *loop-test-times* 100) ; for unsafe queues
(defparameter *loop-times* 100) ; for safe queues

(defparameter *dequeue-list* (list))
(defparameter *enqueue-list* (list))
(defparameter *enqueue-sum* (make-atomic 0))
(defparameter *dequeue-sum* (make-atomic 0))
(defparameter *send-to-push-list* nil)
(defparameter *pop-list* nil)
