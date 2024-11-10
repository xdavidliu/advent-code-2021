(defun less-op (a b)
  (if (< a b) 1 0))

(defun equal-op (a b)
  (if (= a b) 1 0))

;; hyperspec says CASE uses "same", where "same" is EQL, not EQ, so numbers work.
(defun from-code (code)
  (case code
    (1 #'+)
    (2 #'*)
    (7 #'less-op)
    (8 #'equal-op)))

(defstruct computer
  mem ptr input output phase-input)

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

(defun get-opcode (cmp)
  (mod (ptr-val cmp) 100))

(defun exec-op (cmp)
  (let ((op (from-code (get-opcode cmp)))
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

(defun get-input (cmp)
  (let ((phase (computer-phase-input cmp)))
    (if phase
	(progn (setf (computer-phase-input cmp) nil)
	       phase)
	(computer-input cmp))))

(defun exec-input (cmp)
  (let ((dest (ptr-val cmp 1)))
    (setf (elt (computer-mem cmp) dest)
	  (get-input cmp))
    (incf (computer-ptr cmp) 2)))

(defun exec-output (cmp)
  (let ((bit (get-bit cmp 100))
	(val (ptr-val cmp 1)))
    (setf (computer-output cmp)
	  (getval-mode cmp bit val))
    (incf (computer-ptr cmp) 2)))

(defun cond-jump (cmp)
  (let* ((bit1 (get-bit cmp 100))
	 (param (getval-mode cmp bit1 (ptr-val cmp 1))))
    (if (case (get-opcode cmp)
	  (5 (/= 0 param))
	  (6 (= 0 param)))
	(setf (computer-ptr cmp)
	      (let ((bit2 (get-bit cmp 1000)))
		(getval-mode cmp bit2 (ptr-val cmp 2))))
	(incf (computer-ptr cmp) 3))))

(defun run-once (cmp)
  (case (get-opcode cmp)
    (99 (return-from run-once))
    ((1 2 7 8) (exec-op cmp))
    ((5 6) (cond-jump cmp))
    (3 (exec-input cmp))
    (4 (exec-output cmp))
    (t (error "run-once")))
  t)

(defun run (cmp)
  (when (run-once cmp)
    (run cmp)))
