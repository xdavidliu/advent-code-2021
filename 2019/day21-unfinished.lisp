(load "~/Documents/aoc/util.lisp")
(load "~/Documents/aoc/intcode.lisp")

(defparameter ascii-prog
"NOT D J
WALK
")

(defun run (ascii-prog)
  (let* ((input-vec (read-input "~/Documents/aoc/input21.txt"))
	 (cmp (new-computer input-vec))
	 (in-prog (map 'list 'char-code ascii-prog)))
    (setf (computer-input cmp) (car in-prog)
	  in-prog (cdr in-prog))
    (do (outs (prev t))
        ((null prev) (map 'string 'code-char (nreverse outs)))
      (case (setf prev (run-til-io-or-end cmp))
	(in (when in-prog
	      (setf (computer-input cmp) (car in-prog)
		    in-prog (cdr in-prog))))
	(out (push (computer-output cmp) outs))))))
