(load "~/Documents/aoc/util.lisp")

(defparameter *grid* (read-some-lines "~/Documents/aoc/input10.txt"))

(defun direction (i k r c)
  (let* ((dr (- r i))
	 (dc (- c k))
	 (g (gcd dr dc)))
    (list (/ dr g) (/ dc g))))

(defun do-count (grid i k)
  (let ((seen (make-hash-table :test 'equal)))
    (dotimes (r (length grid) seen)
      (dotimes (c (length (elt grid 0)))
	(when (and (or (/= i r) (/= k c))
		   (eql #\# (elt (elt grid r) c)))
	  (push (list r c) (gethash (direction i k r c) seen)))))))

(defun find-best (grid)
  (let ((best 0) (best-i nil) (best-k nil) (best-seen nil))
    (dotimes (i (length grid) (values best-i best-k best-seen))
      (dotimes (k (length (elt grid 0)))
	(when (eql #\# (elt (elt grid i) k))
	  (let* ((seen (do-count grid i k))
		 (count (hash-table-count seen)))
	    (when (< best count)
	      (setf best count best-seen seen best-i i best-k k))))))))

(defvar *best-i*)
(defvar *best-k*)
(defvar *best-seen*)

(defun square (x) (* x x))

(defun make-hypot-square-func (i k)
  (lambda (pair)
    (+ (square (- i (car pair)))
       (square (- k (cadr pair))))))

(defun sort-values-by-dist-from-best (best-i best-k seen)
  (maphash (lambda (k v)
	     (declare (ignore k))
	     (sort v #'< :key (make-hypot-square-func best-i best-k)))
	   seen))

(multiple-value-bind (best-i best-k best-seen)
    (find-best *grid*)
  (setf *best-i* best-i *best-k* best-k *best-seen* best-seen)
  (format t "part 1 = ~A~%" (hash-table-count best-seen))
  (sort-values-by-dist-from-best best-i best-k best-seen))  ;; for part 2
;; 282

(defun singleton-if-present (key table)
  (multiple-value-bind (val ok)
      (gethash key table)
    (declare (ignore val))
    (and ok (list key))))

;; maphash is actually for-each-hash
(defun key-list (table)
  (let ((out nil))
    (maphash (lambda (k v) (declare (ignore v))
	       (push k out))
	     table)
    out))

(defun sign-match-func (pair)
  (lambda (other)
    (and (= (signum (car pair))
	    (signum (car other)))
	 (= (signum (cadr pair))
	    (signum (cadr other))))))

(defun filter-by-signs (sn1 sn2 lst)
  (remove-if-not (sign-match-func (list sn1 sn2)) lst))
;; not delete, because cannot be destructive since need to repeat 4 times

(defun sort-by-ratio (keys)
  (sort keys #'< :key (lambda (pair) (/ (car pair) (cadr pair)))))

(defun clockwise-order (table)
  (let ((keys (key-list table)))
    (append (singleton-if-present (list -1 0) table)
	    (sort-by-ratio (filter-by-signs -1 1 keys))
	    (singleton-if-present (list 0 1) table)
	    (sort-by-ratio (filter-by-signs 1 1 keys))
	    (singleton-if-present (list 1 0) table)
	    (sort-by-ratio (filter-by-signs 1 -1 keys))
	    (singleton-if-present (list 0 -1) table)
	    (sort-by-ratio (filter-by-signs -1 -1 keys)))))

(defun make-circular (keys)
  (let ((out (list 'head))
	(cpy (copy-seq keys)))
    (setf (cdr out) cpy)
    (setf (cdr (last cpy)) out)
    out))

(defun part2-answer (pair)
  (+ (car pair) (* 100 (cadr pair))))

(defun loop-thru (circ nth seen)
  (cond ((eq 'head (cadr circ))
	 (loop-thru (cdr circ) nth seen))
	((= nth 200)
	 (format t "part 2 = ~A~%"
		 (part2-answer (car (gethash (cadr circ) seen)))))
	(t (pop (gethash (cadr circ) seen))
	   (if (null (gethash (cadr circ) seen))
	       (progn
		 (setf (cdr circ) (cddr circ))
		 (loop-thru circ (1+ nth) seen))
	       (loop-thru (cdr circ) (1+ nth) seen)))))

(let ((keys (clockwise-order *best-seen*)))
  (loop-thru (make-circular keys) 1 *best-seen*))
;; part 2 = 1008
