(defparameter *input* "#..#...#.....##...#.#.###")
(defparameter *sample* "....##..#.#..##..#..#....")

(defun ind (i j) (+ j (* i 5)))

(defun copy-into (dest src)
  (assert (= (length dest) (length src)))
  (dotimes (i (length dest))
    (setf (elt dest i) (elt src i))))

(defparameter *dirs*
  '((1 0) (-1 0) (0 1) (0 -1)))

(defun is-valid (i j)
  (and (<= 0 i) (< i 5)
       (<= 0 j) (< j 5)))

(defun count-bugs (grid i j)
  (let ((found 0))
    (dolist (d *dirs*)
      (let ((r (+ i (car d)))
	    (c (+ j (cadr d))))
	(when (and (is-valid r c)
		   (eql #\# (elt grid (ind r c))))
	  (incf found)
	  (when (= 3 found)
	    (return-from count-bugs found)))))
    found))

(defun next-ch (grid i j)
  (let ((c (count-bugs grid i j)))
    (case (elt grid (ind i j))
      (#\# (if (= 1 c) #\# #\.))
      (#\. (if (find c '(1 2)) #\# #\.)))))

(defun update-all (prev now)
  (dotimes (i 5)
    (dotimes (j 5)
      (setf (elt now (ind i j)) (next-ch prev i j)))))

(defun solve (input)
  (let* ((src (make-string (length input)))
	 (dest (make-string (length input)))
	 (seen (make-hash-table :test 'equal)))
    (copy-into src input)
    (setf (gethash (copy-seq input) seen) t)
    (do () (nil)
      (update-all src dest)
      (when (gethash dest seen)
	(format t "part 1 = ~A~%" (rating dest))
	(return-from solve))
      (setf (gethash (copy-seq dest) seen) t)
      (rotatef src dest))))

(defun rating (grid)
  (let ((pow 1) (total 0))
    (dotimes (i (length grid))
      (when (eql #\# (elt grid i))
	(incf total pow))
      (setf pow (* 2 pow)))
    total))

(solve *input*)
;; 24662545

(defun make-part2-array ()
  (make-array '(450 16) :initial-element #\.))

;; upper-left corner is index 0
(defun populate (arr in grid)
  (let ((out (1+ in)))
    (dotimes (j 5)
      ;; top row left to right
      (setf (aref arr out j) (elt grid (ind 0 j))
      ;; bottom row right to left
            (aref arr out (+ 8 j)) (elt grid (ind 4 (- 4 j)))))
    (dotimes (j 3)
      ;; right face up to down
      (setf (aref arr out (+ 5 j)) (elt grid (ind (1+ j) 4))
      ;; left face down to up
	    (aref arr out (+ 13 j)) (elt grid (ind (- 3 j) 0))))
    (dotimes (j 3)
      ;; inner top row left to right
      (setf (aref arr in j) (elt grid (ind 1 (1+ j)))
      ;; inner bottom row right to left
	    (aref arr in (+ 4 j)) (elt grid (ind 3 (- 3 j)))))
    (setf (aref arr in 3) (elt grid (ind 2 3))
	  (aref arr in 7) (elt grid (ind 2 1)))))

#|
     |     |         |     |     
  0  |  1  |    2    |  3  |  4  
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |         |     |     
 15  |  0  |    1    |  2  |  5 
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |A|B|C|D|E|     |     
     |     |-+-+-+-+-|     |     
     |     |F|G|H|I|J|     |     
     |     |-+-+-+-+-|     |     
 14  |  7  |K|L|?|N|O|  3  |  6 
     |     |-+-+-+-+-|     |     
     |     |P|Q|R|S|T|     |     
     |     |-+-+-+-+-|     |     
     |     |U|V|W|X|Y|     |     
-----+-----+---------+-----+-----
     |     |         |     |     
 13  |  6  |    5    |  4  |  7 
     |     |         |     |     
-----+-----+---------+-----+-----
     |     |         |     |     
 12  | 11  |    10   |  9  |  8 
     |     |         |     |     
|#

(defparameter eight-neighbors
  #(((0 1) (0 7) (1 1) (1 15))
    ((0 0) (0 2) (1 2)
     (-1 0) (-1 1) (-1 2) (-1 3) (-1 4))
    ((0 1) (0 3) (1 3) (1 5))
    ((0 2) (0 4) (1 6)
     (-1 4) (-1 5) (-1 6) (-1 7) (-1 8))
    ((0 3) (0 5) (1 7) (1 9))
    ((0 4) (0 6) (1 10)
     (-1 8) (-1 9) (-1 10) (-1 11) (-1 12))
    ((0 5) (0 7) (1 11) (1 13))
    ((0 0) (0 6) (1 14)
     (-1 12) (-1 13) (-1 14) (-1 15) (-1 0))))

(defparameter sixteen-neighbors
  #(((0 1) (0 15) (1 1) (1 7))
    ((0 0) (0 2) (-1 0) (1 1))
    ((0 1) (0 3) (-1 1) (1 1))
    ((0 2) (0 4) (-1 2) (1 1))
    ((0 3) (0 5) (1 1) (1 3))
    ((0 4) (0 6) (-1 2) (1 3))
    ((0 5) (0 7) (-1 3) (1 3))
    ((0 6) (0 8) (-1 4) (1 3))
    ((0 7) (0 9) (1 3) (1 5))
    ((0 8) (0 10) (-1 4) (1 5))
    ((0 9) (0 11) (-1 5) (1 5))
    ((0 10) (0 12) (-1 6) (1 5))
    ((0 11) (0 13) (1 5) (1 7))
    ((0 12) (0 14) (-1 6) (1 7))
    ((0 13) (0 15) (-1 7) (1 7))
    ((0 0) (0 14) (-1 0) (1 7))))

(defun count-from-layer (src i j)
  (let ((ns (elt (if (evenp i)
		     sixteen-neighbors
		     eight-neighbors)
		 j))
	(c 0))
    (dolist (nb ns)
      (let ((ch (aref src (+ i (car nb)) (cadr nb))))
	(when (eql #\# ch)
	  (incf c)
	  (when (= 3 c) (return-from count-from-layer c)))))
    c))

(defun compute-val (src i j)
  (let ((c (count-from-layer src i j)))
    (case (aref src i j)
      (#\# (if (= 1 c) #\# #\.))
      (#\. (if (find c '(1 2)) #\# #\.)))))

(defun update-layer (src dest i)
  (let ((top (if (evenp i) 16 8)))
    (dotimes (j top)
      (setf (aref dest i j) (compute-val src i j)))))

(defun count-bugs (arr)
  (let ((c 0))
    (dotimes (i (array-dimension arr 0))
      (dotimes (j (array-dimension arr 1))
	(when (eq #\# (aref arr i j))
	  (incf c))))
    c))

(let ((src (make-part2-array))
      (dest (make-part2-array))
      (minutes 200)
      (input *input*)
      (in 225) (out 226))
  (populate src in input)
  (dotimes (not-used minutes)
    (decf in)
    (incf out)
    (do ((c in (1+ c)))
	((> c out))
      (update-layer src dest c))
    (rotatef src dest))
  (format t "part 2 = ~A~%" (count-bugs src)))
;; 2063
