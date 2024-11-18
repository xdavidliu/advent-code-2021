(load "~/Documents/aoc/util.lisp")

(defun item-pair (str)
  (let ((p (split " " str)))
    (cons (parse-integer (car p))
	  (cdr p))))

(defun reaction-list (line)
  (let* ((x (split " => " line))
	 (y (split ", " (car x))))
    (mapcar #'item-pair (cons (cadr x) y))))

(defun add-to-table (table rl)
  (setf (gethash (cadar rl) table) rl))

(defun make-reaction-table (filename)
  (let ((table (make-hash-table :test 'equal)))
    (with-open-file (strm filename)
      (do ((line (read-line strm nil) (read-line strm nil)))
	  ((null line))
	(add-to-table table (reaction-list line))))
    (sort-table-values table)))

(defun get-height (r-table dp-table elem)
  (if (string= elem "ORE")
      0
      (or (gethash elem dp-table)
	  (let ((ans (1+ (apply #'max (mapcar (lambda (c)
						(get-height r-table dp-table (cadr c)))
					      (cdr (gethash elem r-table)))))))
	    (setf (gethash elem dp-table) ans)
	    ans))))

(defun sort-table-values (r-table)
  (let ((out (make-hash-table :test 'equal))
	(dp-table (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
	       (setf (gethash k out)
		     (sort v #'> :key (lambda (x)
					(get-height r-table dp-table (cadr x))))))
	     r-table)
    out))
;; sort is destructive but can't guarantee that it correctly sorts in place if I
;; directly do it on r-table. Also can't setf to v while maphash because it might
;; invalidate iterators or whatev. Hence we make a defensive copy.

(defun make-leftovers-table (r-table)
  (let ((out (make-hash-table :test 'equal)))
    (setf (gethash "ORE" out) 0)
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (setf (gethash k out) 0))
	     r-table)
    out))

(defun ore-needed (n elem left r-table)
  (if (string= elem "ORE")
      n
      (let ((le (gethash elem left)))
	(when (null le)
	  (setf le 0 (gethash elem left) 0))
	(cond ((<= n le)
	       (decf (gethash elem left) n)
	       0)
	      (t (decf n le)
		 (let* ((val-elem (gethash elem r-table))
			(rhs-n (caar val-elem))
			(r-mult (ceiling n rhs-n)))
		   (setf (gethash elem left) (- (* r-mult rhs-n) n))
		   (reduce #'+ (mapcar (lambda (x)
					 (ore-needed (* r-mult (car x))
						     (cadr x)
						     left
						     r-table))
				       (cdr val-elem)))))))))

(defun ore-for-fuel (n r-table)
  (let ((left (make-hash-table :test 'equal)))
    (ore-needed n "FUEL" left r-table)))

(defun part1 ()
  (let ((r-table (make-reaction-table "~/Documents/aoc/input14.txt")))
    (format t "part 1 = ~A~%" (ore-for-fuel 1 r-table))))
;; 870051

(part1)

(format t "part 2 = 1863741~%")
;; just manually binary search using below until get largest number smaller than
;; 1 trillion
;; (ore-for-fuel 1863741 *r-table*)
