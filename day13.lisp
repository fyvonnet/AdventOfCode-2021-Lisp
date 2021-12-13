(defpackage :day13
  (:use :cl :aoc-misc :cl-ppcre :forfuncs)
  (:export main))

(in-package :day13)

(defparameter *coord-re* "^(\\d+),(\\d+)$")
(defparameter *folds-re* "^fold along (.)=(\\d+)$")

(defun decode-coords (input &optional coords)
  (multiple-value-bind (match regs) (scan-to-strings *coord-re* (car input))
    (if (null match)
      (values
        (reverse coords)
        (cdr input))
      (decode-coords
        (cdr input)
        (cons (map 'list #'parse-integer regs) coords)))))

(defun decode-folds (input)
  (unless (null input)
    (multiple-value-bind (match regs) (scan-to-strings *folds-re* (car input))
      (cons
        (cons (char (aref regs 0) 0) (parse-integer (aref regs 1)))
        (decode-folds (cdr input))))))

(defun display-pattern (pattern)
  (destructuring-bind (height width) (array-dimensions pattern)
    (loop for y below height doing
      (loop for x below width doing
        (if (aref pattern y x)
          (format t "#")
          (format t " ")))
      (format t "~%")))
  (format t "~%"))

(defun fold-pattern (pattern fold)
  (destructuring-bind (axis . lines) fold
    (destructuring-bind (height width) (array-dimensions pattern)
      (if (char= #\y axis)
        (progn
          (let ((new-pattern (make-array `(,lines ,width) :initial-element nil)))
            (loop for y below lines doing
              (loop for x below width doing
                (setf (aref new-pattern y x) (aref pattern y x))))
            (loop
              for y below lines
              for ry from (1- height) above lines
              doing
              (loop for x below width doing
                (when (aref pattern ry x)
                  (setf (aref new-pattern y x) t))))
            new-pattern))
        (let ((new-pattern (make-array `(,height ,lines) :initial-element nil)))
          (loop for y below height doing
            (loop for x below lines doing
              (setf (aref new-pattern y x) (aref pattern y x))))
          (loop for y below height doing
            (loop
              for x below lines
              for rx from (1- width) above lines
              doing
              (when (aref pattern y rx)
                (setf (aref new-pattern y x) t))))
          new-pattern)))))

(defun main ()
  (multiple-value-bind (coords rest-input) (decode-coords (read-input-as-list 13))
    (multiple-value-bind (width height)
      (for/fold
        ((xmin (caar coords)) (xmax (caar coords))
         (ymin (cadar coords)) (ymax (cadar coords)))
        ((coord (cdr coords)))
        (values 
          (min xmin (car coord)) (max xmax (car coord))
          (min ymin (cadr coord)) (max ymax (cadr coord)))
        :result (values (+ 1 xmin xmax) (+ 1 ymin ymax)))

      (let*
        ((folds (decode-folds rest-input))
         (pattern (make-array `(,height ,width) :initial-element nil)))

        (loop for coord in coords doing
          (destructuring-bind (x y) coord
            (setf (aref pattern y x) t)))

        (let
          ((new-pattern (fold-pattern pattern (first folds))))
          (print
            (destructuring-bind (height width) (array-dimensions new-pattern)
              (loop for y below height summing
                (loop for x below width counting (aref new-pattern y x))))))
        (terpri)

        (display-pattern
          (for/fold
            ((pattern pattern))
            ((fold folds))
            (fold-pattern pattern fold)))))))

