(asdf:defsystem #:cl-fast-queues
  :author "He Xiangzhi <hexiangzhi@gmail.com>"
  :licence "MIT"
  :version "1.4.6"
  :description "cl-fast-queues implements arrays based, optimized unbounded LIFO and FIFO queues for both unsafe and safe accessing."
  :serial t
  :in-order-to ((test-op (test-op "cl-fast-queues/tests")))
  :depends-on (:bordeaux-threads
               :alexandria
               :atomics
               :cl-speedy-lifo)
  :components ((:file "packages")
               (:file "specials")
               (:file "generics")
               (:file "speedy-queue")
               (:file "speedy-queue-safe")
               (:file "list-queue")
               (:file "unsafe-queues")
               (:file "safe-queues")))


(defsystem "cl-fast-queues/tests"
  :author "He Xiang-zhi"
  :license "MIT"
  :version "1.4.5"
  :serial t
  :depends-on (:cl-fast-queues
               :parachute)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "specials")
                             (:file "speedy-queue")
                             (:file "speedy-queue-safe")
                             (:file "list-queue")
                             (:file "unsafe-fifo")
                             (:file "unsafe-lifo")
                             (:file "safe-fifo")
                             (:file "safe-lifo"))))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :cl-fast-queues-tests)))
