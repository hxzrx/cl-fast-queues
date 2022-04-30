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
