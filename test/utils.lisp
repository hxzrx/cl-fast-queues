(in-package :cl-fast-queues-tests)

(defun nshuffle (sequence)
  "Knuth shuffle. https://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp"
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun make-atomic (init-value)
  "Return a structure that can be cas'ed"
  #+ccl
  (make-array 1 :initial-element init-value)
  #-ccl
  (cons init-value nil))

(defmacro atomic-place (atomic-structure)
  "Return value of atomic-fixnum in macro."
  #+ccl
  `(svref ,atomic-structure 0)
  #-ccl
  `(car ,atomic-structure))

(defmacro atomic-incf (place &optional (diff 1))
  "Atomic incf fixnum in `place' with `diff' and return OLD value."
  #+sbcl
  `(sb-ext:atomic-incf ,place ,diff)
  #+ccl
  `(let ((old ,place))
     (ccl::atomic-incf-decf ,place ,diff)
     old))

(defun make-random-list (len &optional (max 5))
  (loop for i below len
        collect (random max)))
