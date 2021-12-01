(defpackage :day01
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day01)

(defun count-increases (measurements)
  (if (= (length measurements) 1)
    0
    (+
      (if (< (first measurements) (second measurements)) 1 0)
      (count-increases (cdr measurements)))))

(defun sums-of-three (measurements)
  (unless (= (length measurements) 2)
    (cons
      (+ (first measurements) (second measurements) (third measurements))
      (sums-of-three (cdr measurements)))))

(defun main ()
  (let
    ((input (read-input-as-list 1 #'parse-integer )))
    (dolist (func '(identity sums-of-three))
      (print (count-increases (funcall func input))))))

