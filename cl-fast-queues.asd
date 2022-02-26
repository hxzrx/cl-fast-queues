
(asdf:defsystem #:cl-fast-queues
  :author "He Xiangzhi <hexiangzhi@gmail.com>"
  :licence "MIT"
  :version "1.0.1"
  :description "cl-fast-queues implement non-consing, optimized unbounded LIFO and FIFO queues"
  :serial t
  :in-order-to ((test-op (test-op "cl-fast-queues/tests")))
  :depends-on (:bordeaux-threads
               :cl-speedy-lifo)
  :components ((:file "packages")
               (:file "speedy-queue")
               (:file "unsafe-queues")
               (:file "safe-queues")))

(defsystem "cl-fast-queues/tests"
  :author "He Xiang-zhi"
  :license "MIT"
  :version "1.0.1"
  :serial t
  :depends-on (:cl-fast-queues
               :parachute)
  :components ((:file "tests"))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :cl-fast-queues-tests)))
