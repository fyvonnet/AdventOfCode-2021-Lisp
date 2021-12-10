(defpackage :day01
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :forfuncs :for/count :for/list))

(in-package :day01)

(defun count-increases (measurements)
  (for/count
    ((a      measurements)
     (b (cdr measurements)))
    (< a b)))

(defun sums-of-three (measurements)
  (unless (= (length measurements) 2)
    (cons
      (destructuring-bind (a b c &rest _) measurements (+ a b c))
      (sums-of-three (cdr measurements)))))

(defun main ()
  (let ((measurements (read-input-as-list 1 #'parse-integer)))
    (print (count-increases measurements))
    (print
      (count-increases
        (for/list
          ((a       measurements)
           (b (cdr  measurements))
           (c (cddr measurements)))
          (+ a b c))))))

