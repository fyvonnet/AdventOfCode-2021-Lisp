(defpackage :day02
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split))

(in-package :day02)

(defun decode (line)
  (destructuring-bind (cmd x) (split " " line)
    (cons (intern cmd "KEYWORD") (parse-integer x))))

(defun solve (input forw-func down-func up-func)
  (destructuring-bind (h d _)
    (reduce
      (lambda (data cmd)
        (destructuring-bind (h d a) data
          (funcall
            (case (car cmd)
              (:|forward| forw-func)
              (:|down|    down-func)
              (:|up|        up-func))
            h d a (cdr cmd))))
      input :initial-value '(0 0 0))
    (print (* h d))))

(defun main ()
  (let
    ((input (read-input-as-list 02 #'decode)))

    (solve 
      input
      (lambda (h d a x) (list (+ h x) d    0))
      (lambda (h d a x) (list    h (+ d x) 0))
      (lambda (h d a x) (list    h (- d x) 0)))

    (solve 
      input
      (lambda (h d a x) (list (+ h x) (+ d (* a x))    a   ))
      (lambda (h d a x) (list    h       d          (+ a x)))
      (lambda (h d a x) (list    h       d          (- a x))))))

