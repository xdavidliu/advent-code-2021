(defstruct moon pos (vel (list 0 0 0)))

(defun update-vel (a b)
  (dotimes (i 3)
    (let ((qa (elt (moon-pos a) i))
	  (qb (elt (moon-pos b) i)))
      (cond ((> qa qb)
	     (decf (elt (moon-vel a) i))
	     (incf (elt (moon-vel b) i)))
	    ((< qa qb)
	     (incf (elt (moon-vel a) i))
	     (decf (elt (moon-vel b) i)))))))

(defun update-all-vel (moons)
  (dotimes (i 4)
    (dotimes (k 4)
      (when (< i k)
	(update-vel (elt moons i) (elt moons k))))))

(defun update-pos (m)
  (dotimes (i 3)
    (incf (elt (moon-pos m) i)
	  (elt (moon-vel m) i))))

(defun potential (m)
  (reduce #'+ (mapcar #'abs (moon-pos m))))

(defun kinetic (m)
  (reduce #'+ (mapcar #'abs (moon-vel m))))

(defun total-energy (moons)
  (reduce #'+
	  (mapcar (lambda (m)
		    (* (potential m)
		       (kinetic m)))
		  moons)))

(defparameter *moons*
  (list (make-moon :pos (list 9 13 -8))
	(make-moon :pos (list -3 16 -17))
	(make-moon :pos (list -4 11 -10))
	(make-moon :pos (list 0 -2 -2))))

(defun take-one-step (moons)
  (update-all-vel moons)
  (dolist (m moons)
    (update-pos m)))

(dotimes (i 1000)
  (take-one-step *moons*))

(format t "part 1 = ~A~%" (total-energy *moons*))
;; 7758

(defun get-key (moons k)
  (append (mapcar (lambda (m) (elt (moon-pos m) k)) moons)
	  (mapcar (lambda (m) (elt (moon-vel m) k)) moons)))

(defun singleton-table (moons k)
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash (get-key moons k) table) 0)
    table))

(defun get-starting-tables (moons)
  (mapcar (lambda (k) (singleton-table moons k)) '(0 1 2)))

(defparameter *huge* 1000000000000)

(defun find-cycle-bounds (moons)
  (let ((seen (get-starting-tables moons))
	(count-done 0))
    (dotimes (iter *huge*)
      (take-one-step moons)
      (dotimes (i 3)
	(let ((seen-i (elt seen i))
	      (key (get-key moons i)))
	  (when (hash-table-p seen-i)
	    (multiple-value-bind (val ok)
		(gethash key seen-i)
	      (cond (ok (setf (elt seen i) (list val (1+ iter)))
			(when (= 3 (incf count-done))
			  (return-from find-cycle-bounds seen)))
		    (t (setf (gethash key seen-i) (1+ iter)))))))))))

(defun pair-diff (p) (- (cadr p) (car p)))

(defun first-repeat (bounds)
  (let ((start (apply #'max (mapcar #'car bounds)))
	(period (apply #'lcm (mapcar #'pair-diff bounds))))
    (+ start period)))

(format t "part 2 = ~A~%" (first-repeat (find-cycle-bounds *moons*)))
;; 354540398381256
