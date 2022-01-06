(defpackage :day17
  (:use :cl :aoc-misc :cl-ppcre :forfuncs :trivia :iterate)
  (:export main)
  (:import-from :alexandria :iota)
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
         ((vel-y (iota 201 :start -100))) ; [-100..100]
         (match (move-probe-y target-area vel-y)
           (nil (values max-y vel-ys))
           (n (values (max max-y n) (cons vel-y vel-ys))))
         :result
         (progn
           (print max-y)
           vel-ys)))
     (vel-xs
       (reduce
         (lambda (vel-ys vel-y)
           (match (move-probe-x target-area vel-y)
             (nil vel-ys)
             (n (cons n vel-ys))))
         (iota 300) :initial-value nil)))

    (print
      (iterate outer
        (for vel-y in vel-ys)
        (iterate
          (for vel-x in vel-xs)
          (in outer (counting (move-probe target-area vel-x vel-y))))))))

