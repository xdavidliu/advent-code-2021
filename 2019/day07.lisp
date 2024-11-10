(load "~/Documents/intcode.lisp")
(import '(new-computer run-once computer-input computer-output))

(load "~/Documents/util.lisp")
(defparameter *input-vec* (read-input "~/Documents/input07.txt"))

(defun nc (phase)
  (let ((cmp (new-computer *input-vec*)))
    (setf (computer-phase-input cmp) phase)
    cmp))

(defun run-til-output-or-end (cmp)
  (let ((opcode (get-opcode cmp)))
    (cond ((= 99 opcode) nil)
	  ((= 4 opcode) (run-once cmp) t)
	  (t (run-once cmp)
	     (run-til-output-or-end cmp)))))

(defun try-perm (a b c d e)
  (let ((cmps (list (nc a) (nc b) (nc c) (nc d) (nc e))))
    (setf (computer-input (elt cmps 0)) 0)
    (dolist (amp-ptr (list 0 1 2 3 4))
      (when (> amp-ptr 0)
	(setf (computer-input (elt cmps amp-ptr))
	      (computer-output (elt cmps (1- amp-ptr)))))
      (run-til-output-or-end (elt cmps amp-ptr)))
    (computer-output (elt cmps 4))))
      
(defun try-all-perms (try-func start-perm)
  (do ((perm-output)
       (best-output -1000 (max best-output perm-output))
       (perm start-perm)  ;; for clarity, optional
       (next-perm-result t (next-perm perm)))
      ((null next-perm-result) best-output)
    (setf perm-output (apply try-func (coerce perm 'list)))
    best-output))

(let ((part1 (try-all-perms #'try-perm (vector 0 1 2 3 4))))
  (format t "part 1 = ~A~%" part1))

(defun try-with-looping (a b c d e)
  (let ((cmps (list (nc a) (nc b) (nc c) (nc d) (nc e))))
    (setf (computer-input (elt cmps 0)) 0)
    (loop-til-end cmps 0)
    (computer-output (elt cmps 4))))

;; hack: need to setf output to nil before calling run-til-output-or-end
;; we don't want to blow away output at 4 otherwise we don't know answer
(defparameter *last-known-last-output* nil)

(defun loop-til-end (cmps amp-ptr)
  (when (run-til-output-or-end (elt cmps amp-ptr))
    (let ((next-ptr (mod (1+ amp-ptr) 5)))
      (setf (computer-input (elt cmps next-ptr))
	    (computer-output (elt cmps amp-ptr)))
      (loop-til-end cmps next-ptr))))

(let ((part2 (try-all-perms #'try-with-looping (vector 5 6 7 8 9))))
  (format t "part 2 = ~A~%" part2))
;; 39431233
