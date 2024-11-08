(defun split-orbit (line)
  (let ((p (position #\) line)))
    (values (subseq line 0 p) (subseq line (1+ p)))))

(defun read-adj (filename)
  (with-open-file (strm filename)
    (do ((line (read-line strm nil) (read-line strm nil))
	 (acc nil (cons line acc)))
	((null line) (make-adj acc)))))

;; one only has one child, never more than one
;; so it's not really a tree
(defun make-adj (lines)
  (let ((adj (make-hash-table :test 'equal)))
    (dolist (line lines adj)
      (multiple-value-bind (child parent)
	  (split-orbit line)
	(setf (gethash parent adj) child)))))

(defun compute (key desc-count adj)
  (multiple-value-bind (value present)
      (gethash key desc-count)
    (if present
	value
	(multiple-value-bind (child adj-present)
	    (gethash key adj)
	  (if (not adj-present)
	      0
	      (let ((total (1+ (compute child desc-count adj))))
		(setf (gethash key desc-count) total)
		total))))))

(defparameter *adj* (read-adj "~/Documents/input.txt"))

(let ((desc-count (make-hash-table :test 'equal)))
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (compute k desc-count *adj*))
	   *adj*)
  (let ((total 0))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (incf total v))
	     desc-count)
    (format t "part 1 = ~A~%" total)))
;; 119831

(defun make-two-adj (one-adj)
  (let ((two-adj (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
	       (push k (gethash v two-adj))
	       (push v (gethash k two-adj)))
	     one-adj)
    two-adj))

(defun make-queue ()
  (cons nil nil))

(defun enqueue (q x)
  (push x (car q)))

(defun dequeue (q)
  (if (null (cdr q))
      (do ()
	  ((null (car q)))
	(push (pop (car q)) (cdr q))))
  (pop (cdr q)))

(defun queue-empty (q)
  (and (null (car q))
       (null (cdr q))))

(defun make-singleton-queue (elem)
  (let ((q (make-queue)))
    (enqueue q elem)
    q))

(defun bfs (two-adj src dest)
  (do ((q (make-singleton-queue (cons src 0)))
       (seen (make-hash-table :test 'equal)))
      ((queue-empty q) (error "queue unexpectedly empty"))
    (let* ((next (dequeue q))
	   (one-more (1+ (cdr next))))
      (dolist (neighbor (gethash (car next) two-adj))
	(cond ((string= dest neighbor)
	       (return-from bfs one-more))
	      ((null (gethash neighbor seen))
	       (enqueue q (cons neighbor one-more))
	       (setf (gethash neighbor seen) t)))))))

(let ((src (gethash "YOU" *adj*))
      (dest (gethash "SAN" *adj*))
      (two-adj (make-two-adj *adj*)))
  (format t "part 2 = ~A~%" (bfs two-adj src dest)))
;; 322
