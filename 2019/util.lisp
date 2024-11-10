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

;; permutations functions

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
