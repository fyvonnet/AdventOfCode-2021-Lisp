(defpackage :day01
  (:use :cl :aoc-misc)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day01)

(defmacro count-increases (dest num1 num2)
  ` (nlet rec ((measurements input) (count 0))
      (destructuring-bind (,@dest &rest rest) measurements
        (let ((new-count (if (< ,num1 ,num2) (1+ count) count)))
          (if (null rest)
            (format t "~D~%" new-count)
            (rec (cdr measurements) new-count))))))

(defun main ()
  (let ((input (read-input-as-list 1 #'parse-integer)))
    (count-increases (a b) a b)
    (count-increases (a b c d) (+ a b c) (+ b c d))))

