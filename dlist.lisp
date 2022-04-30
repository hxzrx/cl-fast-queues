;;;; https://rosettacode.org/wiki/Doubly-linked_list/Definition#Common_Lisp


(in-package :dlist)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (node (:constructor %make-node (content prev next)))
    content prev next) ; node definition

  (defvar *null-node*
    (%make-node nil nil nil)
    "The head of a dlist is a null node , the tail of a dlist is a null.")

  (defstruct (dlist (:constructor %make-dlist (head tail))) ; doubly linked list definition
    head tail))

(defun inspect-node (node)
  (if (eq node *null-node*)
      "NULL-NODE"
      (format nil "~d" (node-content node))))
(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream (inspect-node node))))

(defun make-node (content prev next)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (%make-node content prev next))


(defun inspect-dlist (dlist)
  (format nil "Contents: (~{~d~^ ~})" (dlist-elements dlist)))
#+:ignore
(defmethod print-object ((dlist dlist) stream)
  (print-unreadable-object (dlist stream :type t :identity t)
    (format stream (inspect-dlist dlist))))

(defun make-dlist (&rest contents)
  "Make a doubly-linked list with initial contents."
  ;; (make-dlist 1 2 3 4)
  (let ((dlist (%make-dlist nil nil)))
    (dolist (content contents)
      (insert-tail dlist content))
    dlist))

(defun node-content-compare (node1 node2 &optional (test #'equal))
  "Compare the contents of two node objects using equal."
  #+:ignore(declare (node node1 node2)
           (function test))
  (funcall test (node-content node1)
           (node-content node2)))

(defun insert-between (dlist before after data)
  "Insert a fresh link containing DATA after existing link BEFORE if not nil and before existing link AFTER if not nil.
Note that all node between BEFORE and AFTER will be lost."
  #+:ignore(declare (dlist dlist)
           (node before after))
  (let ((new-node (make-node data before after)))
    (if (null before)
        (setf (dlist-head dlist) new-node
              (node-prev new-node) *null-node*)
        (setf (node-next before) new-node))
    (if (null after)
        (setf (dlist-tail dlist) new-node
              (node-next new-node) *null-node*)
        (setf (node-prev after) new-node))
    new-node))

(defun insert-before (dlist node data)
  "Insert a fresh link containing DATA before existing link NODE"
  ;(declare (dlist dlist) (node node))
  (insert-between dlist (node-prev node) node data))

(defun insert-after (dlist node data)
  "Insert a fresh link containing DATA after existing link NODE"
  ;(declare (dlist dlist) (node node))
  (insert-between dlist node (node-next node) data))

(defun insert-head (dlist data)
  "Insert a fresh link containing DATA at the head of DLIST"
  ;(declare (dlist dlist) (node node))
  (insert-between dlist nil (dlist-head dlist) data))

(defun insert-tail (dlist data)
  "Insert a fresh link containing DATA at the tail of DLIST"
  ;(declare (dlist dlist) (node node))
  (insert-between dlist (dlist-tail dlist) nil data))

(defun remove-node (dlist node)
  "Remove link NODE from DLIST and return its content"
  ;(declare (dlist dlist) (node node))
  (let ((before (node-prev node))
        (after (node-next node)))
    (if (eq *null-node* before)
        (setf (dlist-head dlist) after)
        (setf (node-next before) after))
    (if (eq *null-node* after)
        (setf (dlist-tail dlist) before)
        (setf (node-prev after) before))))

(defun remove-tail (dlist)
  ;;(declare (dlist dlist))
  (remove-node dlist (dlist-tail dlist)))

(defun node-head-p (node)
  "Test if node is the head of some dlist."
  ;(declare (node node))
  (eq (node-prev node) *null-node*))
(defun node-tail-p (node)
  "Test if node is the tail of some dlist."
  ;(declare (node node))
  (eq (node-next node) *null-node*))

(defun dlist-head-p (dlist node)
  (eq node (dlist-head dlist)))
(defun dlist-tail-p (dlist node)
  (eq node (dlist-tail dlist)))

(defun dlist-elements (dlist)
  "Returns the elements of DLIST as a list"
  (if (dlist-empty-p dlist)
      nil
      (labels ((extract-values (node acc)
                 (if (eq (node-next node) *null-node*)
                     (cons (node-content node) acc)
                     (extract-values (node-next node) (cons (node-content node) acc)))))
        (let ((head (dlist-head dlist)))
          (reverse (extract-values head nil))))))

(defun dlist-length (dlist)
  "Return the count of the nodes of DLIST"
  ;;(dlist-length (make-dlist 1 2 3 4 5))
  ;(declare (dlist dlist))
  (if (dlist-empty-p dlist)
      0
      (loop for node = (dlist-head dlist) then (node-next node)
            for len fixnum from 0
            until (eq node *null-node*)
            finally (return len))))

(defun dlist-single-p (dlist)
  "Return true if DLIST has only one node, else return nil."
  (and (eq (dlist-head dlist) (dlist-tail dlist))
       (dlist-head dlist)))

(defun dlist-empty-p (dlist)
  "Return true if DLIST has none nodes."
  ;(declare (dlist dlist))
  (null (dlist-head dlist)))

(defun dlist-to-list (dlist)
  (dlist-elements dlist))

(defun list-to-dlist (content-list)
  (apply #'make-dlist content-list))
