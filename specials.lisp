(in-package :cl-fast-queues)

(defvar *overflow-flag* #.cl-speedy-lifo:*overflow-flag*)
(defvar *underflow-flag* #.cl-speedy-lifo:*underflow-flag*)
(defvar *enlarge-size* 1.5)
