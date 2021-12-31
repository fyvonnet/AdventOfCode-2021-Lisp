(defpackage :day01
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :forfuncs :for/count))

(in-package :day01)

(defun count-increases (measurements)
  (for/count
    ((a      measurements)
     (b (cdr measurements)))
    (< a b)))

(defun main ()
  (let ((measurements (read-input-as-list 1 #'parse-integer)))
    (format t "~D~%" (count-increases measurements))
    (format t "~D~%"
            (count-increases
              (loop for a in       measurements
                    for b in (cdr  measurements)
                    for c in (cddr measurements)
                    collect (+ a b c))))))

