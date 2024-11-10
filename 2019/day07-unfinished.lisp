(load "~/Documents/intcode.lisp")
(import '(new-computer run-once computer-input computer-output))

(load "~/Documents/util.lisp")
(defparameter *input-vec* (read-input "~/Documents/input07.txt"))

(defun nc (phase)
  (let ((cmp (new-computer *input-vec*)))
    (setf (computer-phase-input cmp) phase)
    cmp))

(defun run-til-output (cmp)
  (when (null (computer-output cmp))
    (run-once cmp)
    (run-til-output cmp)))

(defun try-perm (a b c d e)
  (let ((cmps (list (nc a) (nc b) (nc c) (nc d) (nc e)))
	(amp-ptr 0))
    (setf (computer-input (elt cmps 0)) 0)
    (dolist (amp-ptr (list 0 1 2 3 4))
      (when (> amp-ptr 0)
	(setf (computer-input (elt cmps amp-ptr))
	      (computer-output (elt cmps (1- amp-ptr)))))
      (run-til-output (elt cmps amp-ptr)))
    (computer-output (elt cmps 4))))
      
(defun try-all-perms (start-perm)
  (do ((perm-output)
       (best-output -1000 (max best-output perm-output))
       (perm start-perm)  ;; for clarity, optional
       (next-perm-result t (next-perm perm)))
      ((null next-perm-result) best-output)
    (setf perm-output (apply #'try-perm (coerce perm 'list)))))

(let ((part1 (try-all-perms (vector 0 1 2 3 4))))
  (format t "part 1 = ~A~%" part1))
