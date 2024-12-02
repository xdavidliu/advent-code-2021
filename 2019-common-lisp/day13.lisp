(load "~/Documents/aoc/intcode.lisp")
(load "~/Documents/aoc/util.lisp")

(defparameter *input-vec* (read-input "~/Documents/aoc/input13.txt"))

(defun tile-char (id)
  (case id (0 #\.) (1 #\W) (2 #\B) (3 #\P) (4 #\O) (otherwise (error "tile-char ~A" id))))

;; use different default tile from #\. to distinguish between explicitly drawn
;; #\. at beginning
(defun draw-game (tiles)
  (draw tiles #\  t))  ;; t is for invert y axis

(defun hack-last-output-mem (cmp)
  (let* ((output-op (memget cmp (- (computer-ptr cmp) 2)))
	 (mode (floor output-op 100))
	 (dest (memget cmp (1- (computer-ptr cmp)))))
    (setval-mode cmp mode dest 1)))

(defun run-game (cmp tiles in-vals &key (draw-each nil) mem0-val
				     print-block-count)
  (when mem0-val (memset cmp 0 mem0-val))
  (setf (computer-input cmp) (car in-vals))
  (let ((out-vals (list 0 0 0))
	(out-pos 0)
	(broken-count 0)
	(block-count 0))
    (do () (nil)
      (case (run-til-io-or-end cmp)
	('in (setf in-vals (cdr in-vals)
		   (computer-input cmp) (car in-vals)))
	('out
	 (setf (elt out-vals out-pos) (computer-output cmp)
	       out-pos (mod (1+ out-pos) 3))
	 (when (= 0 out-pos)
	   (when (= 2 (elt out-vals 2))
	     (incf block-count))
	   (let ((xy (list (elt out-vals 0) (elt out-vals 1))))
	     (if (equal xy (list -1 0))
		 (when (and (> broken-count 0) (= block-count broken-count))
		   (format t "part 2 = ~A~%" (elt out-vals 2))
		   (return-from run-game))
		 (let ((gh (gethash xy tiles))
		       (e2 (elt out-vals 2)))
		   (when (and gh (eql gh #\B) (= e2 0))
		     (incf broken-count))
		   (setf (gethash xy tiles) (tile-char e2)))))
	   (when draw-each (draw-game tiles))))
	(otherwise  ; not nil: https://stackoverflow.com/questions/6098087
	 (when print-block-count
	   (format t "part 1 = ~A~%" block-count))
	 (return-from run-game))))))

(defun hack-paddle-row-into-wall (cmp)
  (let* ((vec (computer-mem cmp))
	 (loc (search '(0 3 0) (computer-mem cmp)))
	 (left (position 1 vec :from-end t :end loc))
	 (right (position 1 vec :start loc)))
    (do ((i (1+ left) (1+ i)))
	((= i right))
      (setf (elt vec i) 1))))

(defun circ-zeros ()
  (let ((out (list 0)))
    (setf (cdr out) out)
    out))

(let ((cmp (new-computer *input-vec*))
      (tiles (make-hash-table :test 'equal)))
  (run-game cmp tiles (circ-zeros) :print-block-count t))
;; 273

(let ((cmp (new-computer *input-vec*))
      (tiles (make-hash-table :test 'equal)))
  (hack-paddle-row-into-wall cmp)
  (run-game cmp tiles (circ-zeros) :draw-each nil :mem0-val 2))
;; 13140
