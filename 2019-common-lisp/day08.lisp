(defvar *grid*)

(with-open-file (strm "~/Documents/input08.txt")
  (setf *grid* (read-line strm)))

(defparameter *width* 25)
(defparameter *height* 6)
(defparameter *chunksize* (* *height* *width*))

(defun begin-fewest-zero (grid chunksize)
  (do ((l 0 r)
       (r chunksize (+ r chunksize))
       (fewest chunksize)
       (fewest-l -1))
      ((= r (length grid)) fewest-l)
    (let ((zeros (count #\0 (subseq grid l r))))
      (when (< zeros fewest)
	(setf fewest zeros)
	(setf fewest-l l)))))

(defun one-two-prod (grid chunksize)
  (let* ((fewest-l (begin-fewest-zero grid chunksize))
	 (r (+ chunksize fewest-l))
	 (ones (count #\1 (subseq grid fewest-l r)))
	 (twos (count #\2 (subseq grid fewest-l r))))
    (* ones twos)))

(format t "part 1 = ~A~%" (one-two-prod *grid* *chunksize*))
;; 2356

(defun color-char (num-char)
  (case num-char
    (#\0 #\black_large_square)
    (#\1 #\white_large_square)
    (otherwise (error "color-char"))))

(defun draw-pixel (grid canvas start chunksize)
  (do ((i start (+ i chunksize)))
      ((or (>= i (length grid))
	   (not (eql #\2 (elt grid i))))
       (when (< i (length grid))
	 (setf (elt canvas (mod i chunksize))
	       (color-char (elt grid i)))))))

(defun draw-layers (grid chunksize)
  (let ((canvas (make-string chunksize :initial-element #\ )))
    (dotimes (i chunksize)
      (draw-pixel grid canvas i chunksize))
    canvas))

(defun display (canvas height width)
  (dotimes (i height)
    (let* ((l (* i width))
	   (r (+ l width)))
      (format t "~A~%" (subseq canvas l r)))))

(format t "part 2 =~%")
(let ((canvas (draw-layers *grid* *chunksize*)))
  (display canvas *height* *width*))
;; PZEKB
