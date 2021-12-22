(defpackage :day22
  (:use :cl :aoc-misc :cl-ppcre)
  (:export main))

(in-package :day22)

(defun parse2 (line)
  (destructuring-bind (onoff rest) (split " " line)
    (cons
      (string= onoff "on")
      (apply
        #'append
        (mapcar
          (lambda (str)
            (multiple-value-bind (_ regs) (scan-to-strings ".=(-?\\d+)\.\.(-?\\d+)" str)
              (map 'list #'parse-integer regs)))
          (split "," rest))))))

(defun main ()
  (let
    ((input (read-input-as-list 22 #'parse2))
     (array (make-array '(101 101 101) :initial-element nil))
     (cubes 0))

    (loop for step in input doing
      (destructuring-bind (turn-on xa xb ya yb za zb) step
        (let
          ((xmin (max -50 (min xa xb)))
           (xmax (min  50 (max xa xb)))
           (ymin (max -50 (min ya yb)))
           (ymax (min  50 (max ya yb)))
           (zmin (max -50 (min za zb)))
           (zmax (min  50 (max za zb))))
          (loop for z from zmin to zmax doing
            (loop for y from ymin to ymax doing
              (loop for x from xmin to xmax doing
                (let*
                  ((xx (+ 50 x))
                   (yy (+ 50 y))
                   (zz (+ 50 z))
                   (is-on (aref array zz yy xx)))
                  (cond
                    ((and      turn-on  (not is-on)) (incf cubes))
                    ((and (not turn-on)      is-on ) (decf cubes)))
                  (setf (aref array zz yy xx) turn-on))))))))

    (print cubes)))

