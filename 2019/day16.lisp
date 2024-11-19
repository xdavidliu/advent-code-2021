(defun cumulate (v)
  (dotimes (i (1- (length v)))
    (incf (elt v (1+ i)) (elt v i))))

(defun use-sign (pos x)
  (if pos x (- x)))

(defun right-digit (n)
  (mod (abs n) 10))

(defun skip (j src)
  (if (zerop j)
      (skip-0 src)
      (let ((jmp (* 2 (1+ j))))
	(do ((k (1- j) (+ k jmp))
	     (pos t (not pos))
	     (sum 0))
	    ((>= k (1- (length src)))
	     (right-digit sum))
	  (let* ((r (min (1- (length src)) (+ k 1 j)))
		 (diff (- (elt src r) (elt src k))))
	    (incf sum (use-sign pos diff)))))))

(defun skip-0 (src)
  (do ((i 0 (+ 2 i))
       (pos t (not pos))
       (sum 0))
      ((>= i (length src)) (right-digit sum))
    (incf sum (use-sign pos (elt src i)))))

(defun dig-vec (str)
  (map 'vector 'digit-char-p str))

(defun iter (src dest)
  (dotimes (i (length src))
    (when (= 1 i) (cumulate src))
    (setf (elt dest i) (skip i src))))

(defparameter *input*
  (with-open-file (strm "~/Documents/aoc/input16.txt")
    (read-line strm)))

(defparameter *original-src* (dig-vec *input*))

(defun repeat-vector (vec n)
  (let* ((vec-length (length vec))
	 (result-length (* vec-length n))
	 (result (make-array result-length)))
    (dotimes (i n)
      (replace result vec :start1 (* i vec-length)))
    result))

(defun part1 ()
  (let ((src (copy-seq *original-src*))
	(dest (make-array (length *original-src*))))
    (dotimes (i 100)
      (iter src dest)
      (rotatef src dest))
    (format t "part 1 = ~A~%" (subseq src 0 8))))
;; 23135243

(defun part2 (n-times)
  (let* ((src (repeat-vector *original-src* n-times))
	 (dest (make-array (* n-times (length *original-src*))))
	 (off (parse-integer (subseq *input* 0 7))))
    (dotimes (i 100)
      (format t "working on ~A / 100~%" i)
      (iter src dest)
      (rotatef src dest))
    (format t "part 2 = ~A~%" (subseq src off (+ 8 off)))))

;; (part2 10000)
;; 21130597
