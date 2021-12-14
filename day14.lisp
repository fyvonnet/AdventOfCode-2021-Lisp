(defpackage :day14
  (:use :cl :aoc-misc :cl-ppcre :forfuncs)
  (:export main)
  (:import-from :fset :empty-map :with :lookup :convert))

(in-package :day14)

(defun make-rules (input)
  (for/fold 
    ((rules (empty-map)))
    ((line input))
    (multiple-value-bind (_ regs) (scan-to-strings "^(.)(.) -> (.)$" line)
      (destructuring-bind (a b c) (map 'list (lambda (x) (char x 0)) regs)
        (with rules (cons a b) c)))))

(defun run-steps (n rules polymer)
  (if (zerop n)
    polymer
    (for/fold
      ((output `(,(car polymer))))
      ((a polymer)
       (b (cdr polymer)))
      (append
        (list b (lookup rules (cons a b)))
        output)
      :result (run-steps (1- n) rules (reverse output)))))

(defun main ()
  (let*
    ((input (read-input-as-list 14 #'identity))
     (template (coerce (car input) 'list))
     (rules (make-rules (cddr input)))
     (final-polymer (run-steps 10 rules template)))

    (let
      ((count
         (for/fold
           ((map (empty-map 0)))
           ((element final-polymer))
           (with map element (1+ (lookup map element)))
           :result (mapcar #'cdr (convert 'list map)))))
      (for/fold
        ((qt-least (car count))
         (qt-most (car count)))
        ((qt (cdr count)))
        (values
          (min qt qt-least)
          (max qt qt-most))
        :result (print (- qt-most qt-least))))))
