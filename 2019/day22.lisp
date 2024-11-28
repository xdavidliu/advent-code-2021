(load "~/Documents/aoc/util.lisp")

;; wait, for part 1 you don't have to do all the cards
;; you just follow one card and ask what the position

(defun last-token-int (text)
  (let ((last-space (position #\  text :from-end t)))
    (and last-space
	 (parse-integer (subseq text (1+ last-space))
			:junk-allowed t))))

(defun range (n)
  (labels ((f (k acc)
	     (if (zerop k)
		 acc
		 (let ((w (1- k)))
		   (f w (cons w acc))))))
    (f n nil)))

(defun collect-coefs-forward (ins n)
  (let ((a 1) (b 0) c d)
    (dolist (line ins)
      (let ((num (last-token-int line)))
	(cond ((string= line "deal into new stack")
	       (setf c -1 d (1- n)))
	      ((string= "cut" (subseq line 0 3))
	       (setf c 1 d (- num)))
	      ((string= "deal with" (subseq line 0 9))
	       (setf c num d 0)))
	(setf a (mod (* c a) n)
	      b (mod (+ d (* c b)) n))))
    (list a b)))

(defparameter *instructions*
  (read-some-lines "~/Documents/aoc/input22.txt"))

(let* ((n 10007)
       (coefs (collect-coefs-forward *instructions* n))
       (k (+ (cadr coefs) (* 2019 (car coefs)))))
  (format t "part 1 = ~A~%" (mod k n)))

(defun square (x) (* x x))

(defun mod-expt (base pow m)
  (labels
      ((f (b p acc)
	 (if (= 1 p)
	     (mod (* b acc) m)
	     (f (mod (square b) m)
		(floor p 2)
		(if (evenp p) acc (mod (* b acc) m))))))
    (f base pow 1)))

(defun collect-coefs-reverse (ins n)
  (let ((rev (reverse ins))
	(a 1)
	(b 0)
	c d)
    (dolist (line rev)
      (let ((num (last-token-int line)))
	(cond ((string= line "deal into new stack")
	       (setf c -1 d (1- n)))
	      ((string= "cut" (subseq line 0 3))
	       (setf c 1 d num))
	      ((string= "deal with" (subseq line 0 9))
	       (setf c (mod-inverse n num) d 0)))
	(setf a (mod (* c a) n)
	      b (mod (+ d (* c b)) n))))
    (list a b)))

(defun mod-inverse (a b)
  (labels
      ((z (x y)
	 (+ (* x a)
	    (* y b))))
    (do ((x1 1) (y1 0)
	 (x2 0) (y2 1))
	((zerop (z x2 y2))
	 (and (= 1 (z x1 y1))
	      (mod y1 a)))
      (let* ((q (floor (z x1 y1) (z x2 y2)))
	     (x3 (- x1 (* q x2)))
	     (y3 (- y1 (* q y2))))
	(setf x1 x2 y1 y2 x2 x3 y2 y3)))))

(let* ((n 119315717514047)
       (m 101741582076661)
       (coefs (collect-coefs-reverse *instructions* n))
       (a (car coefs))
       (b (cadr coefs))
       (am (mod-expt a m n))
       (bm (mod (* b (1- am) (mod-inverse n (1- a))) n))
       (k 2020)
       (km (mod (+ bm (* am k)) n)))
  (format t "part 2 = ~A~%" km))
;; 27697279941366
       
