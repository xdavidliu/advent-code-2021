(load "~/Documents/aoc/util.lisp")
(load "~/Documents/aoc/intcode.lisp")

(defparameter *input-vec*
  (read-input "~/Documents/aoc/input23.txt"))

(defun create-network ()
  (let* ((n 50)
	 (cmps (make-array n)))
    (dotimes (i n)
      (let ((cmp-i (new-computer *input-vec*)))
	(setf (elt cmps i) cmp-i)
	(setf (computer-input cmp-i) i)
	(assert (eq 'in (run-til-io-or-end cmp-i)))
	(setf (computer-input cmp-i) -1)))
    cmps))

(defun solve ()
  (let* ((cmps (create-network))
	 (n (length cmps))
	 (ques (make-array n))
	 part1-done
	 x-nat
	 y-nat
	 prev-y-nat)
    (dotimes (i n) (setf (elt ques i) (make-queue)))
    (do ((idle t t)) (nil)
      (do ((i 0 (1+ i)))
	  ((= i n))
	(when (and (= -1 (computer-input (elt cmps i)))
		   (not (queue-empty (elt ques i))))
	  (setf idle nil)
	  (setf (computer-input (elt cmps i))
		(dequeue (elt ques i))))
	(case (run-til-io-or-end (elt cmps i))
	  (in
	   (when (/= -1 (computer-input (elt cmps i)))
	     (setf idle nil
		   (computer-input (elt cmps i)) -1)))
	  (out
	   (setf idle nil)
	   (let (x y (address (computer-output (elt cmps i))))
	     (assert (eq 'out (run-til-io-or-end (elt cmps i))))
	     (setf x (computer-output (elt cmps i)))
	     (assert (eq 'out (run-til-io-or-end (elt cmps i))))
	     (setf y (computer-output (elt cmps i)))
	     (cond ((= 255 address)
		    (when (not part1-done)
		      (format t "part 1 = ~A~%" y)
		      (setf part1-done t))
		    (setf x-nat x y-nat y))
		   (t (let ((destq (elt ques address)))
			(enqueue destq x)
			(enqueue destq y))))))
	  (otherwise (error "unexpected run result"))))
      (when (and idle x-nat y-nat)
	(when (and prev-y-nat (= prev-y-nat y-nat))
	  (format t "part 2 = ~A~%" y-nat)
	  (return-from solve))
	(setf prev-y-nat y-nat)
	(enqueue (elt ques 0) x-nat)
	(enqueue (elt ques 0) y-nat)))))

;; part 1 = 19530
;; part 2 = 12725
