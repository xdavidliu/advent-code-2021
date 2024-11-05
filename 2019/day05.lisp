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
	   
(defun change (vec ptr)
  (let ((code (mod (elt vec ptr) 100)))
    (case code
      ;; 99 implicitly ends without tail call
      ((1 2 7 8)
       (change-math (from-code code) vec ptr)
       (change vec (+ 4 ptr)))
      ((5 6) (change vec (jump code vec ptr)))
      ((3 4)
       (change-io code vec ptr)
       (change vec (+ 2 ptr))))))

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

(defun change-io (code vec ptr)
  (let ((val (elt vec (1+ ptr))))
    (case code
      ;; input always writes, never immediate
      (3 (setf (elt vec val) *input-value*))
      (4 (let ((mode (hundreds (elt vec ptr))))
	   (push (get-value mode vec (1+ ptr)) *output-values*))))))

(let ((vec (copy-seq *input-vec*))
      (*input-value* 1))
  (change vec 0)
  (format t "part 1 = ~A~%" (car *output-values*)))
;; 13547311

(let ((vec (copy-seq *input-vec*))
      (*input-value* 5))
  (change vec 0)
  (format t "part 2 = ~A~%" (car *output-values*)))
;; 236453
