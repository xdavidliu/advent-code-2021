(load "~/Documents/aoc/intcode.lisp")
(load "~/Documents/aoc/util.lisp")
(defparameter *input-vec* (read-input "~/Documents/aoc/input09.txt"))

(let ((cmp (new-computer *input-vec*)))
  (setf (computer-input cmp) 1)
  (run cmp)
  (format t "part 1 = ~A~%" (computer-output cmp)))
;; 3409270027

(let ((cmp (new-computer *input-vec*)))
  (setf (computer-input cmp) 2)
  (run cmp)
  (format t "part 2 = ~A~%" (computer-output cmp)))
;; 82760
