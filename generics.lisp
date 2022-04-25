(in-package :cl-fast-queues)


(defgeneric enqueue (item queue))

(defgeneric dequeue (queue &optional keep-in-queue-p))

(defgeneric queue-peek (queue))

(defgeneric queue-empty-p (queue))

(defgeneric queue-count (queue))

(defgeneric queue-find (item queue &key key test))

(defgeneric queue-flush (queue))

(defgeneric queue-to-list (queue))

(defgeneric list-to-queue (list queue-type))
