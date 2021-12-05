(defpackage :day05
  (:use :cl :aoc-misc aoc-coord :iterate)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day05)

(defun decode (line)
  (multiple-value-bind (_ regs) (scan-to-strings "^(\\d+),(\\d+) -> (\\d+),(\\d+)+$" line)
    (map 'list #'parse-integer regs)))

(defun seq (a b)
  (cons
    a
    (cond
      ((< a b) (seq (1+ a) b))
      ((> a b) (seq (1- a) b))))) 

(defun main ()
  (let
    ((diagram1 (make-array '(1000 1000) :initial-element 0))
     (diagram2 (make-array '(1000 1000) :initial-element 0)))

    (iterate
      (for s in (read-input-as-list 5 #'decode))
      (destructuring-bind (x1 y1 x2 y2) s
        (cond
          ((= x1 x2)
           (iterate
             (for y in (seq y1 y2))
             (incf (aref diagram1 y x1))
             (incf (aref diagram2 y x1))))
          ((= y1 y2)
           (iterate
             (for x in (seq x1 x2))
             (incf (aref diagram1 y1 x))
             (incf (aref diagram2 y1 x))))
          (t
            (iterate 
              (for y in (seq y1 y2))
              (for x in (seq x1 x2))
              (incf (aref diagram2 y x)))))))

    (dolist (d `(,diagram1 ,diagram2))
      (print
        (iterate
          (for y below 1000)
          (sum
            (iterate
              (for x below 1000)
              (counting (< 1 (aref d y x))))))))))

