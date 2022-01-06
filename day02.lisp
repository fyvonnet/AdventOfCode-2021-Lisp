(defpackage :day02
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split)
  (:import-from :forfuncs :for/fold))

(in-package :day02)

(defun parse (line)
  (destructuring-bind (cmd x) (split " " line)
    (list (intern cmd "KEYWORD") (parse-integer x))))

(defmacro solve (forw-func down-func up-func)
  `(for/fold
     ((h 0) (d 0) (a 0))
     ((step input))
     (destructuring-bind (cmd x) step
       (case cmd
         (:|forward| (values ,@forw-func))
         (:|down|    (values ,@down-func))
         (:|up|      (values ,@up-func))))
     :result (format t "~d~%" (* h d))))

(defun main ()
  (let ((input (read-input-as-list 2 #'parse)))
    (solve ((+ h x) d 0) (h (+ d x) 0) (h (- d x) 0))
    (solve ((+ h x) (+ d (* a x)) a) (h d (+ a x)) (h d (- a x)))))

