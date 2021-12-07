(defpackage :day07
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet))

(in-package :day07)

(defvar triangular)

(defun minmax (lst)
  (nlet rec ((lst (cdr lst)) (min-val (car lst)) (max-val (car lst)))
    (if (null lst)
      (values min-val max-val)
      (rec (cdr lst) (min min-val (car lst)) (max max-val (car lst))))))

(defun min-cost (new-pos max-pos positions func &optional costs)
  (if (> new-pos max-pos)
    (minmax costs)
    (min-cost
      (1+ new-pos)
      max-pos
      positions
      func
      (cons (apply #'+ (mapcar (lambda (p) (funcall func (abs (- new-pos p)))) positions)) costs))))

(defun main ()
  (let ((positions (mapcar #'parse-integer (split "," (car (read-input-as-list 07))))))
    (multiple-value-bind (min-pos max-pos) (minmax positions)
      (setf triangular (coerce (loop :for n :to max-pos :collect (/ (* (1+ n) n) 2)) 'vector))
      (dolist
        (func (list #'identity (lambda (c) (aref triangular c))))
        (print (min-cost min-pos max-pos positions func))))))

