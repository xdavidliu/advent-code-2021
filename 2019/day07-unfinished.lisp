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

(defparameter *input-vec* (read-input "~/Documents/sample.txt"))

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

(defvar *ptr-vec*)
(defvar vec)
(defvar *loop*)
(defvar *phase-used*)
(defvar *phase-ptr*)
(defvar perm)
(defvar *is-first-pass*)

(defun get-ptr ()
  (elt *ptr-vec* *phase-ptr*))

(defun inc-ptr (d)
  (incf (elt *ptr-vec* *phase-ptr*) d))

(defun set-ptr (x)
  (setf (elt *ptr-vec* *phase-ptr*) x))

(defun vec-val ()
  (elt vec (get-ptr)))

(defun change ()
  (format t "change ~A~%" (vec-val))
  (let ((code (mod (vec-val) 100)))
    (case code
      ;; 99 implicitly ends without tail call
      ((1 2 7 8)
       (change-math (from-code code) vec (get-ptr))
       (inc-ptr 4)
       (change))
      ((5 6)
       (set-ptr (jump code vec (get-ptr)))
       (change))
      ((3 4)
       (change-io code)
       (when (= 4 code)
	 (setf *phase-used* nil)
	 (incf *phase-ptr*)
	 (when *loop*
	   (inc-ptr 2)
	   (when (= 5 *phase-ptr*)
	     (setf *is-first-pass* nil)
	     (setf *phase-ptr* 0)))
	 (when (not *loop*)
	   ;; may be optional for part 1
	   (setf vec (copy-seq *input-vec*))
	   (when (= 5 *phase-ptr*)
	     (return-from change nil)))
         (change))
       (when (/= 4 code)
	 (inc-ptr 2)
	 (change))))))

(defun compute-input-value ()
  (cond ((and *is-first-pass* (not *phase-used*))
	 (setf *phase-used* t)
	 (elt perm *phase-ptr*))
	(t *input-value*)))

(defun change-io (code)
  (let ((val (elt vec (1+ (get-ptr)))))
    (case code
      ;; input always writes, never immediate
      ;; line below changed in day 7
      (3 (let ((in-val (compute-input-value)))
	   (format t "input ~A~%" in-val)
	   (setf (elt vec val) in-val)))
      (4 (let ((mode (hundreds (vec-val))))
	   (setf *input-value* (get-value mode vec (1+ (get-ptr)))))))))


;; interestingly, for part 1 I get same result if I don't reset code-vec
;; in the map here, but I'll do so anyway

(defun try-all-perms ()
  (do ((best-output -1000 (max best-output *input-value*))
       (next-perm-result t (next-perm perm)))
      ((null next-perm-result) best-output)
    (setf *input-value* 0)
    (setf *ptr-vec* (vector 0 0 0 0 0))
    (setf *is-first-pass* t)
    (setf *phase-ptr* 0)
    (setf *phase-used* nil)
    (setf vec (copy-seq *input-vec*))
    (change)))

;; (let ((*loop* nil)
;;       (perm (vector 0 1 2 3 4)))
;;   (format t "part 1 = ~A~%" (try-all-perms)))
;; 398674

;; (let ((*loop* t)
;;       (perm (vector 5 6 7 8 9)))
;;   (format t "part 2 = ~A~%" (try-all-perms)))
;; ;;

(let ((*loop* t)
      (perm (vector 9 8 7 6 5)))
  (format t "part 2 = ~A~%" (try-all-perms)))
