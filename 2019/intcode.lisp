(load "~/Documents/util.lisp")
(import '(split read-input))

;; hyperspec says CASE uses "same", where "same" is EQL, not EQ, so numbers work.
(defun from-code (code)
  (case code
    (1 #'+)
    (2 #'*)))

(defstruct computer
  mem ptr input output)

(defun new-computer (input-vec)
  (make-computer :mem (copy-seq input-vec) :ptr 0))

(defun apply-words (cmp noun verb)
  (setf (elt (computer-mem cmp) 1) noun)
  (setf (elt (computer-mem cmp) 2) verb))

(defun ptr-val (cmp &optional (off 0))
  (elt (computer-mem cmp) (+ off (computer-ptr cmp))))  

(defun zeroth-val (cmp)
  (elt (computer-mem cmp) 0))

(defun getval-mode (cmp bit val)
  (case bit
    (0 (elt (computer-mem cmp) val))
    (1 val)
    (otherwise (error "getval-mode"))))

(defun get-bit (cmp place)
  (mod (floor (ptr-val cmp) place) 10))

(defun exec-op (cmp opcode)
  (let ((op (from-code opcode))
	(bit1 (get-bit cmp 100))
	(bit2 (get-bit cmp 1000))
	(left (ptr-val cmp 1))
	(right (ptr-val cmp 2))
	(dest (ptr-val cmp 3))
	(mem (computer-mem cmp)))
    (setf (elt mem dest)
	  (funcall op
		   (getval-mode cmp bit1 left)
		   (getval-mode cmp bit2 right)))
    (incf (computer-ptr cmp) 4)))

(defun exec-input (cmp)
  (let ((dest (ptr-val cmp 1)))
    (setf (elt mem dest)
	  (computer-input cmp))))

(defun exec-output (cmp)
  (let ((bit (get-bit cmp 100))
	(val (ptr-val cmp 1)))
    (setf (computer-output cmp)
	  (getval-mode cmp bit val))))

(defun run (cmp)
  (let ((opcode (mod (ptr-val cmp) 100)))
    (when (not (= 99 opcode))
      (case opcode
	((1 2) (exec-op cmp opcode))
	(3 (exec-input cmp))
	(4 (exec-output cmp)))
      (run cmp))))
