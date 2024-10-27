;; from day 2
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

;; note pos here is 2 el list, pos above is single coord
;; values are segment, next pos, and next dist
(defun segment-and-next (p str d)
  (let ((dir (elt str 0))
	(num (parse-integer (subseq str 1)))
	(x (car p))
	(y (cadr p)))
    (let ((next-d (+ d (abs num))))
      (cond ((eql dir #\R)
	     (values (list 'h y x (+ x num) d)
		     (list (+ x num) y)
		     next-d))
	    ((eql dir #\L)
	     (values (list 'h y x (- x num) d)
		     (list (- x num) y)
		     next-d))
	    ((eql dir #\U)
	     (values (list 'v x y (+ y num) d)
		     (list x (+ y num))
		     next-d))
	    ((eql dir #\D)
	     (values (list 'v x y (- y num) d)
		     (list x (- y num))
		     next-d))))))

(defun pos (seg)
  (cadr seg))

(defun lo-bound (seg)
  (caddr seg))

(defun up-bound (seg)
  (cadddr seg))

(defun dist-of (seg)
  (cadddr (cdr seg))) 

(defun is-horizontal (seg)
  (eq 'h (car seg)))

(defun is-vertical (seg)
  (eq 'v (car seg)))

(defun to-segments (strs)
  (to-segments-help strs (list 0 0) 0 nil))

(defun to-segments-help (strs p d out)
  (if (null strs)
      (nreverse out)
      (multiple-value-bind (seg next-p next-d)
	  (segment-and-next p (car strs) d)
	(to-segments-help (cdr strs) next-p next-d (cons seg out)))))

(defun find-inter (horiz vert)
  (let ((out nil))
    (dolist (h horiz out)
      (dolist (v vert)
	(let ((x (intersect-m-dist h v)))
	  (if x (push x out)))))))

(defun solve-str (str1 str2)
  (solve (split str1 #\,)
	 (split str2 #\,)))

(defun solve (strs1 strs2)
  (let ((segs1 (to-segments strs1))
	(segs2 (to-segments strs2)))
    (let ((horiz1 (remove-if-not 'is-horizontal segs1))
	  (horiz2 (remove-if-not 'is-horizontal segs2))
	  (vert1 (remove-if-not 'is-vertical segs1))
	  (vert2 (remove-if-not 'is-vertical segs2)))
      (min (apply 'min (find-inter horiz1 vert2))
	   (apply 'min (find-inter horiz2 vert1))))))

(defun between-inclusive (a b c)
  (or (and (<= a b) (<= b c))
      (and (<= c b) (<= b a))))

(defparameter *dist-func* nil)

(defun intersect-m-dist (s1 s2)
  (let ((p1 (pos s1))
	(p2 (pos s2)))
    (and (between-inclusive (lo-bound s2) p1 (up-bound s2))
	 (between-inclusive (lo-bound s1) p2 (up-bound s1))
	 (not (and (zerop p1) (zerop p2)))
	 (funcall *dist-func* s1 s2))))
;;	 (+ (abs p1) (abs p2)))))

(defun manhattan (s1 s2)
  (+ (abs (pos s1)) (abs (pos s2))))

(defun path-dist (s1 s2)
  (+ (dist-of s1) (dist-of s2)
     (abs (- (pos s2) (lo-bound s1)))
     (abs (- (pos s1) (lo-bound s2)))))

(with-open-file (strm "~/Documents/input.txt")
  (let* ((line1 (read-line strm))
	 (line2 (read-line strm)))
    (let ((*dist-func* 'manhattan))
      (format t "part1 = ~A~%" (solve-str line1 line2)))
    (let ((*dist-func* 'path-dist))
      (format t "part2 = ~A~%" (solve-str line1 line2)))))
;; part1 768
;; part2 8684
