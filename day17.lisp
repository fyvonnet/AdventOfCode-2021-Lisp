(defpackage :day17
  (:use :cl :aoc-misc :cl-ppcre :forfuncs :trivia)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day17)

(defparameter *re* "target area: x=(\\d+)..(\\d+), y=(-\\d+)..(-\\d+)")

(defun move-probe (target-area x-vel y-vel)
  (destructuring-bind (x-min x-max y-min y-max) target-area
    (nlet rec ((x 0) (y 0) (x-vel x-vel) (y-vel y-vel))
      (unless (or (> x x-max) (< y y-min))
        (if (and (>= x x-min) (<= y y-max))
          t
          (rec 
            (+ x x-vel)
            (+ y y-vel)
            (cond
              ((minusp x-vel) (1+ x-vel))
              ((plusp  x-vel) (1- x-vel))
              ((zerop  x-vel) 0))
            (1- y-vel)))))))

(defun move-probe-y (target-area y-vel-start)
  (destructuring-bind (_ _ y-min y-max) target-area
    (nlet rec ((y 0) (y-vel y-vel-start) (ys nil))
      (unless (< y y-min)
        (let ((new-ys (cons y ys)))
          (if (<= y y-max)
            (apply #'max ys)
            (rec (+ y y-vel) (1- y-vel) new-ys)))))))

(defun move-probe-x (target-area x-vel-start)
  (destructuring-bind (x-min x-max _ _) target-area
    (nlet rec ((x 0) (x-vel x-vel-start))
      (unless (or (zerop x-vel) (> x x-max))
        (if (>= x x-min)
          x-vel-start
          (rec
            (+ x x-vel)
            (cond
              ((minusp x-vel) (1+ x-vel))
              ((plusp  x-vel) (1- x-vel))
              ((zerop  x-vel) 0))))))))


(defun main ()
  (let*
    ((target-area
       (multiple-value-bind (_ regs) (scan-to-strings *re* (car (read-input-as-list 17)))
         (map 'list #'parse-integer regs)))
     (vel-ys
       (for/fold
         ((max-y 0)
          (vel-ys nil))
         ((vel-y (loop for i from -100 to 100 collect i)))
         (match (move-probe-y target-area vel-y)
           (nil (values max-y vel-ys))
           (n (values (max max-y n) (cons vel-y vel-ys))))
         :result
         (progn
           (format t "~d~%" max-y)
           vel-ys)))
     (vel-xs
       (reduce
         (lambda (vel-ys vel-y)
           (match (move-probe-x target-area vel-y)
             (nil vel-ys)
             (n (cons n vel-ys))))
         (loop for i from 0 to 300 collect i)
         :initial-value nil)))

    (print
      (loop for vel-y in vel-ys summing
        (loop
          with counter = 0
          for vel-x in vel-xs 
          when (move-probe target-area vel-x vel-y) do (incf counter)
          finally (return counter))))))

