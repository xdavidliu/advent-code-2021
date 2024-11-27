(load "~/Documents/aoc/util.lisp")
(load "~/Documents/aoc/intcode.lisp")

(defun run (ascii-prog)
  (let* ((input-vec (read-input "~/Documents/aoc/input21.txt"))
	 (cmp (new-computer input-vec))
	 (in-prog (map 'list 'char-code ascii-prog)))
    (setf (computer-input cmp) (car in-prog)
	  in-prog (cdr in-prog))
    (do (outs (prev t))
        ((null prev)
	 (let ((first-out (car outs)))
	   (if (> first-out 256)
	       first-out
	       (map 'string 'code-char (nreverse outs)))))
      (case (setf prev (run-til-io-or-end cmp))
	(in (when in-prog
	      (setf (computer-input cmp) (car in-prog)
		    in-prog (cdr in-prog))))
	(out (push (computer-output cmp) outs))))))

#|
(run "WALK
")

shows that one hole is ##.##

(run "NOT A J
WALK
")

##.#..###

(run "NOT B J
AND A J
WALK
")

###...##

Solution:

V
#...#        

V   V
###.#..#        (D and (not C)) or (not A)

V   V
###.#.#

|#



(format t "part 1 = ~A~%" (run "NOT C T
AND D T
NOT A J
OR T J
WALK
"))
;; 19354818
