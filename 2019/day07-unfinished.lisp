;; from day 5

(defun pos-or-end (item seq &rest args)
  (or (apply #'position item seq args)
      (length seq)))

(defun split (seq delim)
  (do ((l 0 (1+ r))
       (r (pos-or-end delim seq)
	  (pos-or-end delim seq :start (1+ r)))
       (acc nil (cons (subseq seq l r) acc)))
      ((= r (length seq))
       (reverse (cons (subseq seq l r) acc)))))

(defun read-input (fname)
  (with-open-file (strm fname)
    (let ((toks (split (read-line strm) #\,)))
      (coerce (mapcar #'parse-integer toks)
	      'vector))))

(defun less-than (x y)
  (if (< x y) 1 0))

(defun equal-to (x y)
  (if (= x y) 1 0))

(defun from-code (code)
  (case code
    (1 #'+)
    (2 #'*)
    (7 #'less-than)
    (8 #'equal-to)))

(defparameter *input-vec* (read-input "~/Documents/input.txt"))

;; above same as day02

(defun jump (code vec ptr)
  (let* ((header (elt vec ptr))
	 (val1 (get-value (hundreds header) vec (1+ ptr)))
	 (something (case code
		      (5 (not (zerop val1)))
		      (6 (zerop val1)))))
    (if something
	(get-value (thousands header) vec (+ 2 ptr))
	(+ 3 ptr))))

(defun hundreds (num)
  (mod (floor num 100) 10))

(defun thousands (num)
  (mod (floor num 1000) 10))

(defun change-math (op vec ptr)
  (let ((header (elt vec ptr))
	(dest (elt vec (+ 3 ptr))))
    (let ((val1 (get-value (hundreds header) vec (1+ ptr)))
	  (val2 (get-value (thousands header) vec (+ 2 ptr))))
      (setf (elt vec dest) (funcall op val1 val2)))))

(defun get-value (mode vec ptr)
  (let ((val (elt vec ptr)))
    (case mode
      (0 (elt vec val))
      (1 val))))

(defvar *input-value*)
(defparameter *output-values* nil)

;; change-io omitted here, modified below

;; above: from day 5

;; =======================================================

;; new stuff in day 7

;; assumes vec elems unique
(defun bound-index (vec)
  (do ((i (1- (length vec)) (1- i)))
      ((or (zerop i)
	   (< (elt vec (1- i))
	      (elt vec i)))
       i)))

;; assumes there's some elem > piv in vec
(defun successor-index (piv vec)
  (do ((i (1- (length vec)) (1- i)))
      ((< piv (elt vec i))
       i)))

(defun swap-vec-elem (vec i j)
  (if (/= i j)
      (let ((xi (elt vec i))
	    (xj (elt vec j)))
	(setf (elt vec i) xj)
	(setf (elt vec j) xi))))

(defun reverse-from (vec i)
  (do ((l i (1+ l))
       (r (1- (length vec)) (1- r)))
      ((>= l r))
    (swap-vec-elem vec l r)))

(defun next-perm (vec)
  (let ((bi (bound-index vec)))
    (and (not (zerop bi))
	 (let ((si (successor-index (elt vec (1- bi)) vec)))
	   (swap-vec-elem vec (1- bi) si)
	   (reverse-from vec bi)
	   t))))

;; ========================

(defvar *ptr*)
(defvar *code-vec*)
(defvar *phase-used*)
(defvar *continue-looping*)
(defvar *phase-vec*)
(defvar *phase-ptr*)

(defun change ()
  (if (= 5 *phase-ptr*)
      (if *continue-looping*
	  (setf *phase-ptr* 0)
	  (return-from change nil)))
  (let ((code (mod (elt *code-vec* *ptr*) 100)))
    ;; 99 still implicitly terminates
    (case code
      ((1 2 7 8)
       (change-math (from-code code) *code-vec* *ptr*)
       (incf *ptr* 4)
       (change))
      ((5 6)
       (setf *ptr* (jump code *code-vec* *ptr*))
       (change))
      ((3 4)
       (change-io code)
       (incf *ptr* 2)
       (change)))))

(defun compute-input-value ()
  (cond ((not *phase-used*)
	 (setf *phase-used* t)
	 (elt *phase-vec* *phase-ptr*))
	(t *input-value*)))

(defun change-io (code)
  (let ((val (elt *code-vec* (1+ *ptr*))))
    (case code
      (3 (setf (elt *code-vec* val) (compute-input-value)))
      (4 (let ((mode (hundreds (elt *code-vec* *ptr*))))
	   (setf *input-value* (get-value mode *code-vec* (1+ *ptr*)))
	   (incf *phase-ptr*)
	   (if (not *continue-looping*)
	       ;; for part 1, reset software between each amp
	       (setf *ptr* 0)
	       ;; interestingly for part 1 it doesn't seem to make a
	       ;; diffence weather I do copy-seq here or not
	       (setf *code-vec* (copy-seq *input-vec*))))))))

;; still need to reset code-vec and *ptr* in between perms, regardless of
;; value of *reset-software*, since latter is only in between amps.
(defun try-all-perms ()
  (do ((best-output -1000 (max best-output *input-value*))
       (next-perm-result t (next-perm *phase-vec*)))
      ((null next-perm-result) best-output)
    (setf *code-vec* (copy-seq *input-vec*))
    (setf *ptr* 0)
    (setf *phase-ptr* 0)
    (setf *input-value* 0)
    (setf *phase-used* nil)
    (change)))

(let ((*continue-looping* nil)
      (*phase-vec* (vector 0 1 2 3 4)))
  (format t "part 1 = ~A~%" (try-all-perms)))
;; 398674

;; (let ((*continue-looping* t)
;;       (*ptr* 0)
;;       (*phase-vec* (vector 5 6 7 8 9)))
;;   (format t "part 2 = ~A~%" (try-all-perms)))
;; 162 too low

