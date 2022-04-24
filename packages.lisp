
(defpackage :cl-fast-queues
  (:use :cl)
  (:nicknames :fastq)
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
   :queue-find
   :queue-flush
   :queue-to-list
   :list-to-queue
   :*overflow-flag*
   :*underflow-flag*
   :*enlarge-size*
   ))

(defpackage :cl-fast-queues-exp
  (:use :cl)
  (:nicknames :safe-fastq)
  (:export
   :make-safe-fifo
   :make-safe-lifo
   :safe-fifo-p
   :safe-lifo-p
   :enqueue
   :dequeue
   :queue-peek
   :queue-empty-p
   :queue-count
   :queue-find
   :queue-flush
   :queue-to-list
   :list-to-queue
   :*overflow-flag*
   :*underflow-flag*
   :*enlarge-size*
   ))
