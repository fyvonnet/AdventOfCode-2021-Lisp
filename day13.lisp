(defpackage :day13
  (:use :cl :aoc-misc :cl-ppcre :forfuncs)
  (:export main)
  (:import-from :fset :convert :size :empty-set :contains? :with))

(in-package :day13)

(defparameter *coord-re* "^(\\d+),(\\d+)$")
(defparameter *folds-re* "^fold along (.)=(\\d+)$")

(defun parse-folds (input)
  (unless (null input)
    (multiple-value-bind (match regs) (scan-to-strings *folds-re* (car input))
      (cons
        (cons (char (aref regs 0) 0) (parse-integer (aref regs 1)))
        (parse-folds (cdr input))))))

(defun parse (input &optional pattern)
  (multiple-value-bind (match regs) (scan-to-strings *coord-re* (car input))
    (if (null match)
      (values
        pattern
        (parse-folds (cdr input)))
      (parse
        (cdr input)
        (cons (map 'list #'parse-integer regs) pattern)))))

(defun display-pattern (pattern)
  (multiple-value-bind (xmax ymax set)
    (for/fold
      ((xmax (caar pattern))
       (ymax (cadar pattern))
       (set (empty-set)))
      ((coord (cdr pattern)))
      (values
        (max xmax (car coord))
        (max ymax (cadr coord))
        (with set coord)))
    (loop for y to ymax doing
      (loop for x to xmax doing
        (if (contains? set `(,x ,y))
          (format t "â")
          (format t " ")))
      (format t "~%"))
    (format t "~%")))

(defun fold-pattern (fold)
  (destructuring-bind (axis . value) fold
    (if (char= axis #\y)
      (lambda (coord)
        (destructuring-bind (x y) coord
          (if (> y value)
            (list x (- value (- y value)))
            (list x y))))
      (lambda (coord)
        (destructuring-bind (x y) coord
          (if (> x value)
            (list (- value (- x value)) y)
            (list x y)))))))

(defun main ()
  (multiple-value-bind (pattern folds) (parse (read-input-as-list 13))
    (format t "~d~%" (size (convert 'fset:set (mapcar (fold-pattern (first folds)) pattern))))
    (display-pattern
      (for/fold
        ((pattern pattern))
        ((fold folds))
        (mapcar (fold-pattern fold) pattern)))))

