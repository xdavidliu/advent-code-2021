(defun split (delim text)
  (labels ((rec (i acc)
	     (let ((r (search delim text :start2 i)))
	       (if r
		   (rec (+ r (length delim))
			(cons (subseq text i r) acc))
		   (nreverse (cons (subseq text i) acc))))))
    (rec 0 nil)))

(defun read-input (fname)
  (with-open-file (strm fname)
    (let ((toks (split "," (read-line strm))))
      (coerce (mapcar #'parse-integer toks)
	      'vector))))

(defun read-some-lines (filename)
  (with-open-file (strm filename)
    (do ((line (read-line strm nil) (read-line strm nil))
	 (acc nil (cons line acc)))
	((null line) (nreverse acc)))))

(defun debug-table (tab)
  (maphash (lambda (k v)
	     (format t "~A : ~A~%" k v))
	   tab))

(defun debug-grid (grid)
  (map nil (lambda (r) (format t "~A~%" r)) grid))

(defparameter *huge* 10000000000000)

(defun get-bounds (table)
  (let ((min-x *huge*) (max-x (- *huge*))
	(min-y *huge*) (max-y (- *huge*)))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (let ((x (car k)) (y (cadr k)))
		 (setf min-x (min x min-x)
		       max-x (max x max-x)
		       min-y (min y min-y)
		       max-y (max y max-y))))
	     table)
    (values min-x max-x min-y max-y)))

(defun make-black-grid (width height init)
  (let ((grid (make-array height)))
    (dotimes (i height)
      (setf (elt grid i) (make-string width :initial-element init)))
    grid))
;; can't use :initial-element for grid itself because then I think it
;; sets the same string object for each row

(defun draw (table init &optional (rev nil))
  (multiple-value-bind (min-x max-x min-y max-y)
      (get-bounds table)
    (let ((grid (make-black-grid (1+ (- max-x min-x))
				 (1+ (- max-y min-y))
				 init)))
      (maphash (lambda (k v)
		 (let ((k (- (car k) min-x))
		       (i (- max-y (cadr k))))
		   (setf (elt (elt grid i) k) v)))
	       table)
      (when rev (setf grid (nreverse grid)))
      (map nil (lambda (line) (format t "~A~%" line)) grid))))

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

;; queue functions

(defun make-queue ()
  (cons nil nil))

(defun enqueue (q x)
  (push x (car q)))

(defun dequeue (q)
  (if (null (cdr q))
      (do ()
	  ((null (car q)))
	(push (pop (car q)) (cdr q))))
  (pop (cdr q)))

(defun queue-empty (q)
  (and (null (car q))
       (null (cdr q))))

(defun make-singleton-queue (elem)
  (let ((q (make-queue)))
    (enqueue q elem)
    q))
