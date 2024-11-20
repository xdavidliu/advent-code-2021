(load "~/Documents/aoc/intcode.lisp")
(load "~/Documents/aoc/util.lisp")

;; https://stackoverflow.com/a/28681035/2990344
(defun new-arr () (make-array 1 :adjustable t :fill-pointer 0))

(defun output-grid (input-vec)
  (let ((cmp (new-computer input-vec))
	(grid (new-arr))
	(row (new-arr)))
    (do () (nil)
      (when (null (run-til-output-or-end cmp))
	(when (< 0 (length row))
	  (vector-push-extend row grid))
	(return-from output-grid grid))
      (let ((out (computer-output cmp)))
	(case out
	  (10
	   (when (< 0 (length row))
	     (vector-push-extend row grid))
	   (setf row (new-arr)))
	  (otherwise (vector-push-extend out row)))))))

;; debug
(defun print-grid (grid)
  (map nil (lambda (row)
	     (format t "~A~%" (map 'string 'code-char row)))
       grid))

(defun is-star (grid r c val)
  (labels ((match (dr dc)
	     (eql val
		  (elt (elt grid (+ r dr)) (+ c dc)))))
    (and (match 0 0) (match 1 0) (match -1 0) (match 0 1) (match 0 -1))))

(defun sum-alignment (grid)
  (let ((nr (length grid))
	(nc (length (elt grid 0)))
	(sum 0))
    (do ((r 1 (1+ r)))
	((= (1+ r) nr) sum)
      (do ((c 1 (1+ c)))
	  ((= (1+ c) nc))
	(when (is-star grid r c (char-code #\#))
	  (incf sum (* r c)))))))

(defparameter *input-vec* (read-input "~/Documents/aoc/input17.txt"))
(defparameter *grid* (output-grid *input-vec*))

(format t "part 1 = ~A~%" (sum-alignment *grid*))
;; 6000

(defun get-direction (ch)
  (case ch
    (#\^ '(-1 0))
    (#\v '(1 0))
    (#\> '(0 1))
    (#\< '(0 -1))))

(defun find-robot (grid)
  (let ((nr (length grid))
	(nc (length (elt grid 0))))
    (dotimes (r nr)
      (dotimes (c nc)
	(let ((ch (code-char (elt (elt grid r) c))))
	(case ch
	  ((#\. #\#))
	  (otherwise (return-from find-robot (list (get-direction ch) r c)))))))
    (error "find-robot did not return as expected")))

(defun can-move (grid dir r c)
  (let ((nr (length grid))
	(nc (length (elt grid 0)))
	(next-r (+ r (car dir)))
	(next-c (+ c (cadr dir))))
    (and (>= next-r 0) (>= next-c 0)
	 (< next-r nr) (< next-c nc)
	 (= (char-code #\#) (elt (elt grid next-r) next-c)))))

;; https://stackoverflow.com/a/31286211/2990344
;; for why not using defconstant. It's because these are lists, and
;; don't compare EQL in subsequent redefinitions.
(defparameter +all-dirs+ '((-1 0) (0 1) (1 0) (0 -1)))
(defparameter +dir-diffs+ `((-1 #\L) (1 #\R)))

(defun try-turn (grid dir r c)
  (let ((dir-pos (position dir +all-dirs+ :test 'equal)))
    (dolist (s +dir-diffs+)
      (let* ((new-dir-pos (mod (+ dir-pos (car s)) 4))
	     (new-dir (elt +all-dirs+ new-dir-pos)))
	(when (can-move grid new-dir r c)
	  (return-from try-turn (values new-dir (cadr s) t))))))
  '(nil nil nil))

(defun move-forward (grid dir r c)
  (let ((steps-taken 0)
	(dr (car dir))
	(dc (cadr dir)))
    (do () (nil)
      (cond ((can-move grid dir r c)
	     (incf steps-taken)
	     (incf r dr)
	     (incf c dc))
	    (t (return-from move-forward (values steps-taken r c)))))))

(defun travel (acc grid dir r c)
  (multiple-value-bind (next-dir turn-char ok)
      (try-turn grid dir r c)
    (cond ((not ok) (nreverse acc))
	  (t (push turn-char acc)
	     (multiple-value-bind (steps next-r next-c)
		 (move-forward grid next-dir r c)
	       (push steps acc)
	       (travel acc grid next-dir next-r next-c))))))

(defun insert-between-each (lst elem)
  (cond ((null lst) nil)
	((null (cdr lst)) lst)
	(t (cons (car lst)
		 (cons elem
		       (insert-between-each (cdr lst) elem))))))

;; (apply #'travel nil *grid* (find-robot *grid*))
;; (#\R 4 #\L 10 #\L 10 #\L 8 #\R 12 #\R 10 #\R 4 #\R 4 #\L 10 #\L 10 #\L 8 #\R 12 #\R 10 #\R 4 #\R 4 #\L 10 #\L 10 #\L 8 #\L 8 #\R 10 #\R 4 #\L 8 #\R 12 #\R 10 #\R 4 #\L 8 #\L 8 #\R 10 #\R 4 #\R 4 #\L 10 #\L 10 #\L 8 #\L 8 #\R 10 #\R 4)

;; by hand, we see that this is
;; A: #\R 4 #\L 10 #\L 10
;; B: #\L 8 #\R 12 #\R 10 #\R 4
;; C: #\L 8 #\L 8 #\R 10 #\R 4
;; A B A B A C B C A C

;; digit-codes
;; reverse

(defun digit-code (n)
  (char-code (digit-char n)))

;; only works for 0 <= n <= 99
(defun cons-digit-codes-reversed (n lst)
  (if (< n 10)
      (cons (digit-code n) lst)
      (cons (digit-code (mod n 10))  ;; reversed because will reverse later
	    (cons (digit-code (floor n 10)) lst))))

(defun routine-codes (lst)
  (labels ((rec (rest acc)
	     (cond ((null rest) (nreverse acc))
		   ((characterp (car rest))
		    (rec (cdr rest)
			 (cons (char-code (car rest))
			       (cons (char-code #\,) acc))))
		   ((integerp (car rest))
		    (rec (cdr rest)
			 (cons-digit-codes-reversed
			  (car rest)
			  (cons (char-code #\,)
			         acc)))))))
    (cdr (rec lst nil))))
;; cdr is to skip the first comma
;; wait it's reversed, so two cases of routine-codes should have comma after

;; todo: extra 10 newline at end of main
(defun part2-input ()
  (let ((a-list '(#\R 4 #\L 10 #\L 10))
	(b-list '(#\L 8 #\R 12 #\R 10 #\R 4))
	(c-list '(#\L 8 #\L 8 #\R 10 #\R 4))
	(main (map 'list 'char-code "A,B,A,B,A,C,B,C,A,C"))
	(nl (list (char-code #\newline))))
    (append main nl
	    (routine-codes a-list) nl
	    (routine-codes b-list) nl
	    (routine-codes c-list) nl
	    (list (char-code #\n)) nl)))

(defun part2 ()
  (let ((inputs (part2-input))
	(cmp (new-computer *input-vec*)))
    (memset cmp 0 2)
    (setf (computer-input cmp) (car inputs))
    (do () ((null inputs))
      (case (run-til-io-or-end cmp)
	(in
	 (setf inputs (cdr inputs))
	 (setf (computer-input cmp) (car inputs)))))
    (run cmp)
    (format t "output = ~A~%" (computer-output cmp))))
;; 807320

(part2)
