(defpackage :day02
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split)
  (:import-from :forfuncs :for/fold))

(in-package :day02)

(defun decode (line)
  (destructuring-bind (cmd x) (split " " line)
    (list (intern cmd "KEYWORD") (parse-integer x))))

(defmacro solve (forw-func down-func up-func)
  `(for/fold
     ((h 0) (d 0) (a 0))
     ((step input))
     (destructuring-bind (cmd x) step
       (case cmd
         (:|forward| ,forw-func)
         (:|down|    ,down-func)
         (:|up|        ,up-func)))
     :result (format t "~d~%" (* h d))))

(defun main ()
  (let
    ((input (read-input-as-list 02 #'decode)))

    (solve 
      (values (+ h x) d    0)
      (values    h (+ d x) 0)
      (values    h (- d x) 0))

    (solve 
      (values (+ h x) (+ d (* a x))    a   )
      (values    h       d          (+ a x))
      (values    h       d          (- a x)))))

