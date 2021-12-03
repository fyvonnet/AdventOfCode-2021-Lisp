(defpackage :day03
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day03)

(defun bin-to-dec (bits &optional (invert nil))
  (reduce
    (lambda (d b) (+ (if (zerop b) 0 1) (* 2 d)))
    (map 'list (lambda (c) (if (char= c (if invert #\1 #\0)) 0 1)) bits)
    :initial-value 0))

(defun sort-numbers (n numbers &optional (sorted '(nil . nil)))
  (if (null numbers)
    sorted
    (sort-numbers
      n
      (cdr numbers)
      (destructuring-bind (zeros . ones) sorted
        (if (char= #\0 (char (car numbers) n))
          (cons (cons (car numbers) zeros) ones)
          (cons zeros (cons (car numbers) ones)))))))

(defun gamma-rate (numbers &optional (n 0))
  (unless (= n (length (car numbers)))
    (cons
      (destructuring-bind (zeros . ones) (sort-numbers n numbers)
        (if (> (length zeros) (length ones)) #\0 #\1))
      (gamma-rate numbers (1+ n)))))

(defun rating (compare numbers &optional (n 0))
  (if (= 1 (length numbers))
    (bin-to-dec (car numbers))
    (rating
      compare
      (destructuring-bind (zeros . ones) (sort-numbers n numbers)
        (if (funcall compare (length zeros) (length ones)) zeros ones))
      (1+ n))))

(defun main ()
  (let*
    ((numbers (read-input-as-list 3))
     (gamma (gamma-rate numbers)))
    (print (* (bin-to-dec gamma) (bin-to-dec gamma t)))
    (print (* (rating #'> numbers) (rating #'<= numbers)))))

