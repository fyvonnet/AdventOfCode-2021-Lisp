(defpackage :day06
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split))

(in-package :day06)

(defun cycle (fishes)
  (destructuring-bind (zeros &rest rest) fishes
    (incf (nth 6 rest) zeros)
    (append rest `(,zeros))))

(defun run-cycles (n fishes)
  (if (zerop n)
    fishes
    (run-cycles (1- n) (cycle fishes))))

(defun main ()
  (let
    ((input
       (loop :with array = (make-array '(9) :initial-element 0)
             :for f :in (car (read-input-as-list 6 (lambda (l) (mapcar #'parse-integer (split "," l)))))
             :do (incf (aref array f))
             :finally (return (coerce array 'list)))))
    (dolist (n '(80 256))
      (print (apply #'+ (run-cycles n input))))))

