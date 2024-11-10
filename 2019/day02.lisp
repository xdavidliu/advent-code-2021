(load "~/Documents/intcode.lisp")
(import '(new-computer apply-words run zeroth-val))

(defparameter *input-vec* (read-input "~/Documents/input.txt"))

(let ((cmp (new-computer *input-vec*)))
  (apply-words cmp 12 2)
  (run cmp)
  (format t "part 1 = ~A~%" (zeroth-val cmp)))
;; 9581917

;; "Each of the two input values will be between 0 and 99, inclusive."
(defun part2 ()
  (dotimes (noun 100)
    (dotimes (verb 100)
      (let ((cmp (new-computer *input-vec*)))
	(apply-words cmp noun verb)
	(run cmp)
	(when (= 19690720 (zeroth-val cmp))
	  (format t "part 2 = ~A~%" (+ (* 100 noun) verb))
	  (return-from part2))))))

(part2)
;; 2505
