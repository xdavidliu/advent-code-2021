(defun digitp (k)
  (and (<= 0 k) (< k 10)))

(defun dig-internal (out k)
  (if (digitp k)
      (cons k out)
      (dig-internal (cons (mod k 10) out) (floor k 10))))

(defun digits (k)
  (dig-internal nil k))

(defun no-decrease (ks)
  (or (null ks)
      (null (cdr ks))
      (and (<= (car ks) (cadr ks))
	   (no-decrease (cdr ks)))))

(defun has-repeat (ks)
  (and (not (null ks))
       (not (null (cdr ks)))
       (or (= (car ks) (cadr ks))
	   (has-repeat (cdr ks)))))

(defparameter *repeat-func* nil)

(defun is-password (k)
  (let ((ks (digits k)))
    (and (no-decrease ks)
	 (funcall *repeat-func* ks))))

(defparameter *bottom* 193651)
(defparameter *top* 649729)

(defun solve (bottom top)
  (do ((count 0)
       (k bottom (1+ k)))
      ((> k top) (format t "part 1 = ~A~%" count))
    (if (is-password k)
	(incf count))))

(defun part1 (bottom top)
  (let ((*repeat-func* #'has-repeat))
    (solve bottom top)))

(defun part2 (bottom top)
  (let ((*repeat-func* #'has-exactly-two-repeat))
    (solve bottom top)))

(defun next-diff-car (k ks)
  (cond ((null ks) nil)
	((= k (car ks))
	 (next-diff-car k (cdr ks)))
	(t ks)))

(defun has-exactly-two-repeat (ks)
  (and (not (null ks))
       (not (null (cdr ks)))
       (or (only-first-two-equal ks)
	   (has-exactly-two-repeat (next-diff-car (car ks) ks)))))

(defun only-first-two-equal (ks)
  (and (= (car ks) (cadr ks))
       (or (null (cddr ks))
	   (/= (car ks) (caddr ks)))))

(part1 *bottom* *top*)
;; 1605

(part2 *bottom* *top*)
;; 1102
