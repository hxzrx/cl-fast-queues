;;;; https://rosettacode.org/wiki/Doubly-linked_list/Definition#Common_Lisp

(in-package :cl-fast-queues)

(defstruct dlist head tail) ; doubly linked list definition
(defstruct node content prev next) ; node definition

(defun insert-between (dlist before after data)
  "Insert a fresh link containing DATA after existing link BEFORE if not nil and before existing link AFTER if not nil.
Note that all node between BEFORE and AFTER will be lost."
  (let ((new-link (make-node :content data :prev before :next after)))
    (if (null before)
        (setf (dlist-head dlist) new-link)
        (setf (node-next before) new-link))
    (if (null after)
        (setf (dlist-tail dlist) new-link)
        (setf (node-prev after) new-link))
    new-link))

(defun insert-before (dlist node data)
  "Insert a fresh link containing DATA before existing link NODE"
  (insert-between dlist (node-prev node) node data))

(defun insert-after (dlist node data)
  "Insert a fresh link containing DATA after existing link NODE"
  (insert-between dlist node (node-next node) data))

(defun insert-head (dlist data)
  "Insert a fresh link containing DATA at the head of DLIST"
  (insert-between dlist nil (dlist-head dlist) data))

(defun insert-tail (dlist data)
  "Insert a fresh link containing DATA at the tail of DLIST"
  (insert-between dlist (dlist-tail dlist) nil data))

(defun remove-link (dlist node)
  "Remove link NODE from DLIST and return its content"
  (let ((before (node-prev node))
        (after (node-next node)))
    (if (null before)
        (setf (dlist-head dlist) after)
        (setf (node-next before) after))
    (if (null after)
        (setf (dlist-tail dlist) before)
        (setf (node-prev after) before))))

(defun dlist-elements (dlist)
  "Returns the elements of DLIST as a list"
  (labels ((extract-values (node acc)
             (if (null node)
                 acc
                 (extract-values (node-next node) (cons (node-content node) acc)))))
    (reverse (extract-values (dlist-head dlist) nil))))

#+:ignore
(let ((dlist (make-dlist)))
  (insert-head dlist 1)
  (insert-tail dlist 4)
  (insert-after dlist (dlist-head dlist) 2)
  (let* ((next-to-last (insert-before dlist (dlist-tail dlist) 3))
         (bad-link (insert-before dlist next-to-last 42)))
    (remove-link dlist bad-link))
  (print (dlist-elements dlist)))
