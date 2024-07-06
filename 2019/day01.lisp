(defvar nums)

(with-open-file (str "~/Documents/input.txt")
  (do ((elem (read str nil) (read str nil))
       (acc nil (cons elem acc)))
      ((null elem) (setf nums (reverse acc)))))

(defun fuel (mass)
  (+ -2 (floor mass 3)))

(defun deep-fuel-help (acc f)
  (if (<= f 0)
      acc
      (deep-fuel-help (+ acc f) (fuel f))))

(defun deep-fuel (mass)
  (deep-fuel-help 0 (fuel mass)))

(let ((part1 (apply #'+ (mapcar #'fuel nums))))
  (format t "part 1 = ~A~%" part1))
;; 3398090

(let ((part2 (apply #'+ (mapcar #'deep-fuel nums))))
  (format t "part 2 = ~A~%" part2))
;; 5094261
