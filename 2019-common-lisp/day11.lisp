(load "~/Documents/aoc/intcode.lisp")
(load "~/Documents/aoc/util.lisp")
(import '(new-computer read-input))

(defparameter *input-vec* (read-input "~/Documents/aoc/input11.txt"))

(defstruct robot
  cmp pos dir whites painted)

(defun new-robot (program)
  (let ((cmp (new-computer program)))
    (setf (computer-input cmp) 0)
    (make-robot :cmp cmp
		:pos (list 0 0) :dir 0
		:whites (make-hash-table :test 'equal)
		:painted (make-hash-table :test 'equal))))

(defparameter *dir-vals* '((0 1) (1 0) (0 -1) (-1 0)))

(defun paint (rb)
  (let ((pos (robot-pos rb))
	(out (computer-output (robot-cmp rb))))
    (setf (gethash pos (robot-painted rb)) t)
    (case out
      (0 (remhash pos (robot-whites rb)))
      (1 (setf (gethash pos (robot-whites rb)) #\#)))))

(defun turn-dir (rb)
  (let ((out (computer-output (robot-cmp rb)))
	(old-dir (robot-dir rb)))
    (setf (robot-dir rb)
	  (let ((diff (case out (0 3) (1 1))))
	    (mod (+ diff old-dir) 4)))))

(defun move-forward (rb)
  (let ((dir-val (elt *dir-vals* (robot-dir rb)))
	(pos (robot-pos rb))
	(cmp (robot-cmp rb)))
    (setf (robot-pos rb)
	  (list (+ (car pos) (car dir-val))
		(+ (cadr pos) (cadr dir-val))))
    (setf (computer-input cmp)
	  (if (gethash (robot-pos rb) (robot-whites rb))
	      1
	      0))))

(defun run-robot (rb)
  (when (run-til-output-or-end (robot-cmp rb))
    (paint rb)
    (when (run-til-output-or-end (robot-cmp rb))
      (turn-dir rb)
      (move-forward rb)
      (run-robot rb))))

(let ((rb (new-robot *input-vec*)))
  (run-robot rb)
  (format t "part 1 = ~A~%" (hash-table-count (robot-painted rb))))
;; part 1 = 2268

(let ((rb (new-robot *input-vec*)))
  (setf (computer-input (robot-cmp rb)) 1)
  (setf (gethash (list 0 0) (robot-whites rb)) #\#)
  (run-robot rb)
  (draw (robot-whites rb)))
;; CEPKZJCR
