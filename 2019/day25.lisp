(load "~/Documents/aoc/util.lisp")
(load "~/Documents/aoc/intcode.lisp")

(defparameter *input-vec*
  (read-input "~/Documents/aoc/input25.txt"))

(defun game ()
  (let ((cmp (new-computer *input-vec*))
	(ins nil)
	(outs nil))
    (do () (nil)
      (case (run-til-io-or-end cmp)
	(out
	 (push (computer-output cmp) outs)
	 (when (and (eql (car outs) (char-code #\Newline))
		    (cdr outs)
		    (eql (cadr outs) (char-code #\?))
		    (eql (caddr outs) (char-code #\d)))
	   (format t "~A" (map 'string 'code-char (nreverse outs)))
	   (setf outs nil)
	   (setf ins (map 'list 'char-code (read-line)))
	   (setf (computer-input cmp) (car ins))))
	(in
	 (setf ins (cdr ins))
	 (setf (computer-input cmp)
	       ;; because (read-line) doesn't append a newline
	       (if ins (car ins) (char-code #\Newline))))
	(otherwise
	 (format t "~A" (map 'string 'code-char (nreverse outs)))
	 (return-from game))))))

(defparameter *collect-all*
  (list
   "north"
   "take candy cane"  ;; 1
   "south"
   "south"
   "take fuel cell"  ;; 4
   "south"
   "take manifold"  ;; 6
   "north"
   "north"
   "west"
   "take mutex"  ;; 10
   "south"
   "south"
   "take coin"  ;; 13
   "west"
   "take dehydrated water"  ;; 15
   "south"
   "take prime number"  ;; 17
   "north"
   "east"
   "north"
   "east"
   "take cake"  ;; 22
   "north"
   "west"
   "south"
   "west"))

(defparameter *take-locations* '(1 4 6 10 13 15 17 22))

(defun masked-instructions (bits)
  (let (out (loc-i 0) (locs *take-locations*)
	    (ins *collect-all*))
    (dotimes (i (length *collect-all*))
      (cond ((and locs (= i (car locs)))
	     (when (logbitp loc-i bits)
	       (push (car ins) out))
	     (incf loc-i)
	     (setf locs (cdr locs)))
	    (t (push (car ins) out)))
      (setf ins (cdr ins)))
    (nreverse out)))

(defun recorded-game (ins-str)
  (let ((cmp (new-computer *input-vec*))
	(ins (map 'list 'char-code ins-str))
	(outs nil))
    (setf (computer-input cmp) (car ins))
    (do () (nil)
      (case (run-til-io-or-end cmp)
	(in
	 (setf ins (cdr ins))
	 (when ins (setf (computer-input cmp) (car ins))))
	(out
	 (when (not ins)
	   (push (computer-output cmp) outs)
	   (when (and (eql (car outs) (char-code #\Newline))
		      (cdr outs)
		      (eql (cadr outs) (char-code #\?))
		      (eql (caddr outs) (char-code #\d)))
	     (return-from recorded-game
	       (map 'string 'code-char (nreverse outs))))))
	((nil)  ;; hack to get case to match nil
	 (return-from recorded-game
	   (map 'string 'code-char (nreverse outs))))))))

(defun join-with-newlines (instructions)
  ;; https://stackoverflow.com/a/8831029/2990344
  (format nil "~{~A~^~%~}~%" instructions))

(defun recorded-with-mask (b)
  (let* ((mask (masked-instructions b))
	 (str (join-with-newlines mask)))
    (recorded-game str)))

(defun search-for-answer ()
  (dotimes (b 256)
    (format t "working on ~A~%" b)
    (let ((res (recorded-with-mask b)))
      (when (not (or (search "are lighter than" res)
		     (search "are heavier than" res)))
	(format t "~A" res)
	(return-from search-for-answer)))))

(search-for-answer)
;; 278664
