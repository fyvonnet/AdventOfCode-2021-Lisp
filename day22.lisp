(defpackage :day22
  (:use :cl :aoc-misc :cl-ppcre :forfuncs)
  (:import-from :fset :empty-set :with :less :size)
  (:export main))

(in-package :day22)

(defun parse (line)
  (multiple-value-bind (_ regs)
    (scan-to-strings "(\\w+) x=(-?\\d+)\.\.(-?\\d+),y=(-?\\d+)\.\.(-?\\d+),z=(-?\\d+)\.\.(-?\\d+)" line)
    (let ((regs-lst (coerce regs 'list)))
      (cons
        (string= (car regs-lst) "on")
        (mapcar #'parse-integer (cdr regs-lst))))))

(defun main ()
  (let
    ((input (read-input-as-list 22 #'parse)))
    (for/fold
      ((set (empty-set)))
      ((step input))
      (destructuring-bind (turn-on xa xb ya yb za zb) step
        (let
          ((xmin (max -50 (min xa xb)))
           (xmax (min  50 (max xa xb)))
           (ymin (max -50 (min ya yb)))
           (ymax (min  50 (max ya yb)))
           (zmin (max -50 (min za zb)))
           (zmax (min  50 (max za zb)))
           (local-set set))
          (loop for z from zmin to zmax appending
            (loop for y from ymin to ymax appending
              (loop for x from xmin to xmax collecting
                  (if turn-on
                    (setf local-set (with local-set (list x y z)))
                    (setf local-set (less local-set (list x y z)))
                    )))
            finally (return local-set))))
      :result (print (size set)))))

