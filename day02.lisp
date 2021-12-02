(defpackage :day02
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split))

(in-package :day02)

(defun decode (line)
  (destructuring-bind (cmd x) (split " " line)
    (cons (intern cmd "KEYWORD") (parse-integer x))))

(defun navigate (commands &optional (hpos 0) (depth 0))
  (if (null commands)
    (* hpos depth)
    (destructuring-bind (cmd . x) (car commands)
      (destructuring-bind (new-hpos new-depth)
        (case cmd 
          (:|forward| (list (+ hpos x) depth))
          (:|down| (list hpos (+ depth x)))
          (:|up| (list hpos (- depth x))))
        (navigate (cdr commands) new-hpos new-depth)))))

(defun navigate2 (commands &optional (hpos 0) (depth 0) (aim 0))
  (if (null commands)
    (* hpos depth)
    (destructuring-bind (cmd . x) (car commands)
      (destructuring-bind (new-hpos new-depth new-aim)
        (case cmd 
          (:|down| (list hpos depth (+ aim x)))
          (:|up| (list hpos depth (- aim x)))
          (:|forward| (list (+ hpos x) (+ depth (* aim x)) aim)))
        (navigate2 (cdr commands) new-hpos new-depth new-aim)))))

(defun main ()
  (let
    ((input (read-input-as-list 02 #'decode)))
    (print (navigate  input))
    (print (navigate2 input))))

