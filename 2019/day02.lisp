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

;; hyperspec says CASE uses "same", where "same" is EQL, not EQ, so numbers work.
(defun from-code (code)
  (case code
    (1 #'+)
    (2 #'*)))

(defun iterate (vec ptr)
  (let ((op (from-code (elt vec ptr)))
	(ind1 (elt vec (1+ ptr)))
	(ind2 (elt vec (+ 2 ptr)))
	(dest (elt vec (+ 3 ptr))))
    (setf (elt vec dest)
	  (funcall op (elt vec ind1) (elt vec ind2)))))

(defun change (vec)
  (do ((ptr 0 (+ 4 ptr)))
      ((= 99 (elt vec ptr)))
    (iterate vec ptr)))

(defun output (vec noun verb)
  (setf (elt vec 1) noun
	(elt vec 2) verb)
  (change vec)
  (elt vec 0))

(defparameter *input-vec* (read-input "~/Documents/input.txt"))

(let ((vec (copy-seq *input-vec*)))
  (format t "part 1 = ~A" (output vec 12 2)))
;; 9581917

;; "Each of the two input values will be between 0 and 99, inclusive."
(defun part2 ()
  (dotimes (noun 100)
    (dotimes (verb 100)
      (let ((vec (copy-seq *input-vec*)))
	(if (= 19690720 (output vec noun verb))
	    (return-from part2 (+ verb (* 100 noun))))))))

(format t "part 2 = ~A" (part2))
;; 2505
