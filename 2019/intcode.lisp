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
  mem memtable ptr input output phase-input base)

(defun get-or-zero (key table)
  (let ((gotten (gethash key table)))
    (or gotten 0)))

(defun memget (cmp ptr)
  (if (< ptr (length (computer-mem cmp)))
      (elt (computer-mem cmp) ptr)
      (get-or-zero ptr (computer-memtable cmp))))

(defun memset (cmp ptr val)
  (if (< ptr (length (computer-mem cmp)))
      (setf (elt (computer-mem cmp) ptr) val)
      (setf (gethash ptr (computer-memtable cmp)) val)))

(defun new-computer (input-vec)
  (make-computer :mem (copy-seq input-vec) :ptr 0
		 :memtable (make-hash-table) :base 0))

(defun apply-words (cmp noun verb)
  (memset cmp 1 noun)
  (memset cmp 2 verb))

(defun ptr-val (cmp &optional (off 0))
  (memget cmp (+ off (computer-ptr cmp))))

(defun zeroth-val (cmp)
  (memget cmp 0))

(defun getval-mode (cmp mode val)
  (case mode
    (0 (memget cmp val))
    (1 val)
    (2 (memget cmp (+ val (computer-base cmp))))
    (otherwise (error "getval-mode"))))

(defun setval-mode (cmp mode dest val)
  (case mode
    (0 (memset cmp dest val))
    (2 (memset cmp (+ dest (computer-base cmp)) val))
    (otherwise (error "setval-mode"))))

(defun get-mode (cmp place)
  (mod (floor (ptr-val cmp) place) 10))

(defun get-opcode (cmp)
  (mod (ptr-val cmp) 100))

(defun exec-op (cmp)
  (let ((op (from-code (get-opcode cmp)))
	(mode1 (get-mode cmp 100))
	(mode2 (get-mode cmp 1000))
	(mode3 (get-mode cmp 10000))
	(left (ptr-val cmp 1))
	(right (ptr-val cmp 2))
	(dest (ptr-val cmp 3)))
    (let ((result (funcall op
			   (getval-mode cmp mode1 left)
			   (getval-mode cmp mode2 right))))
      (setval-mode cmp mode3 dest result))
    (incf (computer-ptr cmp) 4)))

(defun get-input (cmp)
  (let ((phase (computer-phase-input cmp)))
    (if phase
	(progn (setf (computer-phase-input cmp) nil)
	       phase)
	(computer-input cmp))))

(defun exec-input (cmp)
  (let ((dest (ptr-val cmp 1))
	(mode (get-mode cmp 100)))
    (setval-mode cmp mode dest (get-input cmp))
    (incf (computer-ptr cmp) 2)))

(defun exec-output (cmp)
  (let ((mode (get-mode cmp 100))
	(val (ptr-val cmp 1)))
    (setf (computer-output cmp)
	  (getval-mode cmp mode val))
    (incf (computer-ptr cmp) 2)))

(defun adjust-base (cmp)
  (let ((mode (get-mode cmp 100))
	(val (ptr-val cmp 1)))
    (incf (computer-base cmp)
	  (getval-mode cmp mode val))
    (incf (computer-ptr cmp) 2)))

(defun cond-jump (cmp)
  (let* ((mode1 (get-mode cmp 100))
	 (param (getval-mode cmp mode1 (ptr-val cmp 1))))
    (if (case (get-opcode cmp)
	  (5 (/= 0 param))
	  (6 (= 0 param)))
	(setf (computer-ptr cmp)
	      (let ((mode2 (get-mode cmp 1000)))
		(getval-mode cmp mode2 (ptr-val cmp 2))))
	(incf (computer-ptr cmp) 3))))

(defun run-once (cmp)
  (case (get-opcode cmp)
    (99 (return-from run-once))
    ((1 2 7 8) (exec-op cmp))
    ((5 6) (cond-jump cmp))
    (3 (exec-input cmp))
    (4 (exec-output cmp))
    (9 (adjust-base cmp))
    (t (error "run-once")))
  t)

(defun run-til-output-or-end (cmp)
  (let ((opcode (get-opcode cmp)))
    (cond ((= 99 opcode) nil)
	  ((= 4 opcode) (run-once cmp) t)
	  (t (run-once cmp)
	     (run-til-output-or-end cmp)))))

(defun run (cmp)
  (when (run-once cmp)
    (run cmp)))
