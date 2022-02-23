
(defpackage :cl-fast-queues
  (:use :cl)
  (:export
   :make-unsafe-fifo
   :make-unsafe-lifo
   :make-safe-fifo
   :make-safe-lifo
   :unsafe-fifo-p
   :unsafe-lifo-p
   :safe-fifo-p
   :safe-lifo-p
   :enqueue
   :dequeue
   :queue-peek
   :queue-empty-p
   :queue-count
   :*overflow-flag*
   :*underflow-flag*
   ))
