(defpackage :cl-fast-queues
  (:use :cl)
  (:nicknames :fastq)
  (:export
   :unsafe-fast-fifo
   :unsafe-fast-lifo
   :safe-fast-fifo
   :safe-fast-lifo
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

(defpackage :cl-speedy-queue
  (:use :cl)
  (:export
   :speedy-queue
   :make-queue
   :queue-to-list
   :list-to-queue
   :queue-count
   :queue-length
   :queue-peek
   :queue-full-p
   :queue-empty-p
   :enqueue
   :dequeue
   :queue-find
   :queue-flush
   :*overflow-flag*
   :*underflow-flag*))

(defpackage #:cl-speedy-queue-safe
  (:use :cl)
  (:export
   :safe-speedy-queue
   :make-queue
   :queue-to-list
   :list-to-queue
   :queue-count
   :queue-length
   :queue-peek
   :queue-full-p
   :queue-empty-p
   :enqueue
   :dequeue
   :queue-find
   :queue-flush
   :*overflow-flag*
   :*underflow-flag*
   :*dummy*
   :*max-queue-length*))

(defpackage :dlist
  (:use :cl)
  (:export
   :node
   :make-node
   :node-p
   :node-content
   :node-prev
   :node-next
   :dlist
   :make-dlist
   :dlist-p
   :dlist-head
   :dlist-tail
   :dlist-head-p
   :dlist-tail-p
   :dlist-length
   :dlist-single-p
   :dlist-empty-p
   :insert-between
   :insert-before
   :insert-after
   :insert-head
   :insert-tail
   :remove-node
   :remove-tail
   :dlist-elements
   :dlist-to-list
   :list-to-dlist
   :*null-node*))
