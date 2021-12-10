(defpackage :day10
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :trivia :match)
  (:import-from :forfuncs :for/fold))

(in-package :day10)

(defparameter *opening* "([{<")
(defparameter *closing* ")]}>")
(defparameter *scores* (vector 3 57 1197 25137))

(defun line-score (line &optional stack)
  (if (null line)
    (cons
      'incomplete
      (reduce
        (lambda (s c) (+ (* 5 s) 1 (position c *opening*)))
        stack :initial-value 0))
    (destructuring-bind (fst-char &rest rst-chars) line
      (match (position fst-char *closing*)
        (nil (line-score rst-chars (cons fst-char stack)))
        (c
          (let ((o (position (car stack) *opening*)))
            (if (= o c)
              (line-score rst-chars (cdr stack))
              (cons 'illegal (aref *scores* c)))))))))

(defun main ()
  (for/fold
    ((illegal-score 0)
     (incomplete-scores nil))
    ((score (mapcar #'line-score (read-input-as-list 10 (lambda (l) (coerce l 'list))))))
    (match score
      ((cons 'illegal    s) (values (+ illegal-score s) incomplete-scores))
      ((cons 'incomplete s) (values illegal-score (cons s incomplete-scores))))
    :result
    (progn
      (print illegal-score)
      (print
        (nth
          (floor (length incomplete-scores) 2)
          (sort incomplete-scores #'<))))))

