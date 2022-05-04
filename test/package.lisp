(defpackage #:cl-fast-queues-tests
  (:use #:cl #:parachute #:cl-fast-queues #:dlist)
  (:export nil))

(in-package :cl-fast-queues-tests)

(define-test cl-fast-queues-tests)

(define-test list-queue :parent cl-fast-queues-tests)
;;(define-test dlist :parent cl-fast-queues-tests)

(define-test speedy-queue :parent cl-fast-queues-tests)
(define-test speedy-queue-safe :parent cl-fast-queues-tests)

(define-test unsafe-queues :parent cl-fast-queues-tests)
(define-test unsafe-fifo :parent unsafe-queues)
(define-test unsafe-lifo :parent unsafe-queues)

(define-test safe-queues :parent cl-fast-queues-tests)
(define-test safe-fifo :parent safe-queues)
(define-test safe-lifo :parent safe-queues)


(define-test safe-queues-exp :safe-queues)
(define-test safe-fifo-exp :parent safe-queues-exp)
(define-test safe-lifo-exp :parent safe-queues-exp)
