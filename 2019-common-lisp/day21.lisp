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

W where it jumped
V where it should have jumped

   WV
#####...####..###
    ABCDEFGHIJ


#####.###########    not A
#####...#########

  VABCDEFGH
#####.#..########    A (not C) D H
#####.#.#########
   
    V
#####.#.##...####
    ABCDEFGHI

   V
#####..#.########
    ABCDEFG

   VW  V   V
#####.##.#.#.####   not B  not E not G
    ABCDEFGHI

alternatively, try
not B and D

works!


|#



(format t "part 1 = ~A~%" (run
"NOT C T
AND D T
NOT A J
OR T J
WALK
"))
;; 19354818

(format t "part 2 = ~A~%" (run
"NOT C T
AND A T
AND D T
AND H T
NOT B J
AND D J
OR T J
NOT A T
OR T J
RUN
"))
;; 1143787220
