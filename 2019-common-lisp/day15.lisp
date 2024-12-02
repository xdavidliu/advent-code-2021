(load "~/Documents/aoc/intcode.lisp")
(load "~/Documents/aoc/util.lisp")

(defstruct q-elem
  dist x y ptr mem)

(defun next-pos (input x y)
  (case input
    (1 (list x (1+ y)))
    (2 (list x (1- y)))
    (3 (list (1- x) y))
    (4 (list (1+ x) y))
    (otherwise (error "next-pos"))))

(defun bfs (input-vec x-start y-start ptr)
  (let ((cmp (new-computer nil))
	(q (make-queue))
	(seen (make-hash-table :test 'equal))
	(furthest 0))
    (enqueue q (make-q-elem :dist 0 :x x-start :y y-start
			    :ptr ptr :mem input-vec))
    (setf (gethash (list x-start y-start) seen) t)
    (do () ((queue-empty q) furthest)
      (let ((next (dequeue q)))
	(dolist (in (list 1 2 3 4))
	  (let* ((n-pos (next-pos in (q-elem-x next) (q-elem-y next)))
		 (n-dist (1+ (q-elem-dist next))))
	    (when (null (gethash n-pos seen))
	      (setf (gethash n-pos seen) t)
	      (setf (computer-input cmp) in)
	      (setf (computer-ptr cmp) (q-elem-ptr next))
	      (setf (computer-mem cmp) (copy-seq (q-elem-mem next)))
	      (run-til-output-or-end cmp)
	      ;; doesn't matter whether wall or space, add to seen
	      (case (computer-output cmp)
		(0)  ;; wall, no enqueue
		(2 (return-from bfs
		     (make-q-elem :dist n-dist :x (car n-pos) :y (cadr n-pos)
				  :ptr (computer-ptr cmp)
				  :mem (computer-mem cmp))))
		(1
		 (setf furthest (max furthest n-dist))
		 (enqueue q (make-q-elem
			     :dist n-dist :x (car n-pos) :y (cadr n-pos)
			     :ptr (computer-ptr cmp)
			     :mem (computer-mem cmp))))
		(otherwise (error "bfs case"))))))))))

(let* ((input-vec (read-input "~/Documents/aoc/input15.txt"))
       (el (bfs input-vec 0 0 0))
       (ans1 (q-elem-dist el))
       (x-o (q-elem-x el))
       (y-o (q-elem-y el))
       (ptr-o (q-elem-ptr el))
       (mem-o (q-elem-mem el))
       (res))
  (format t "part 1 = ~A~%" ans1)  ;; 262
  (setf res (bfs mem-o x-o y-o ptr-o))
  (format t "part 2 = ~A~%" res))  ;; 314
