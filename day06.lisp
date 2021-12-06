(defpackage :day06
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split))

(in-package :day06)

(defun cycle (fishes &optional n zeros)
  (cond
    ((null fishes) (list zeros))
    ((null zeros ) (cycle (cdr fishes) 0 (car fishes)))
    (t
      (cons
        (+ (if (= 6 n) zeros 0) (car fishes))
        (cycle (cdr fishes) (1+ n) zeros)))))

(defun run-cycles (n fishes)
  (if (zerop n)
    fishes
    (run-cycles (1- n) (cycle fishes))))

(defun decode (line &optional array fishes)
  (cond
    ((null array)
     (decode
       nil
       (make-array '(9) :initial-element 0)
       (mapcar #'parse-integer (split "," line))))
    ((null fishes) (coerce array 'list))
    (t
      (incf (aref array (car fishes)))
      (decode nil array (cdr fishes)))))

(defun main ()
  (let
    ((input (car (read-input-as-list 6 #'decode))))
    (dolist (n '(80 256))
      (print (apply #'+ (run-cycles n input))))))

