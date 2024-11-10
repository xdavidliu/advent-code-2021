(load "~/Documents/intcode.lisp")
(import '(new-computer run computer-output))

(defparameter *input-vec* (read-input "~/Documents/input05.txt"))

(let ((cmp (new-computer *input-vec*)))
  (setf (computer-input cmp) 1)
  (run cmp)
  (format t "part 1 = ~A~%" (computer-output cmp)))
;; 13547311

(let ((cmp (new-computer *input-vec*)))
  (setf (computer-input cmp) 5)
  (run cmp)
  (format t "part 2 = ~A~%" (computer-output cmp)))
;; 236453
