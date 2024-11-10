(load "~/Documents/util.lisp")
(import '(split read-input))

;; hyperspec says CASE uses "same", where "same" is EQL, not EQ, so numbers work.
(defun from-code (code)
  (case code
    (1 #'+)
    (2 #'*)))

(defstruct computer
  mem ptr)

(defun new-computer (input-vec)
  (make-computer :mem (copy-seq input-vec) :ptr 0))

(defun apply-words (cmp noun verb)
  (setf (elt (computer-mem cmp) 1) noun)
  (setf (elt (computer-mem cmp) 2) verb))

(defun ptr-val (cmp &optional (off 0))
  (elt (computer-mem cmp) (+ off (computer-ptr cmp))))  

(defun zeroth-val (cmp)
  (elt (computer-mem cmp) 0))

(defun exec-op (cmp)
  (let ((op (from-code (ptr-val cmp)))
	(left (ptr-val cmp 1))
	(right (ptr-val cmp 2))
	(dest (ptr-val cmp 3))
	(mem (computer-mem cmp)))
    (setf (elt mem dest)
	  (funcall op (elt mem left) (elt mem right)))
    (incf (computer-ptr cmp) 4)))

(defun stopped (cmp)
  (= 99 (ptr-val cmp)))

(defun run (cmp)
  (when (not (stopped cmp))
    (exec-op cmp)
    (run cmp)))
