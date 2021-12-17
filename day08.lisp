(defpackage :day08
  (:use :cl :aoc-misc :cl-ppcre :forfuncs)
  (:export main))

(in-package :day08)

(defun parse (line)
  (mapcar 
    (lambda (s) (mapcar (lambda (p) (sort (coerce p 'list) #'char<)) s))
    (mapcar
      (lambda (x) (split " " x))
      (split " \\| " line))))

(defmacro find-digit (digit &rest conditions)
  `(let ((found (find-if (lambda (e) (and ,@conditions)) digits)))
     (setf (aref found-digits ,digit) found)
     (setf digits (remove found digits :test #'equal))))

(defmacro has-segments (l)
  `(= ,l (length e)))

(defmacro has-common-segments (n d)
  `(= ,n (length (intersection e (aref found-digits ,d)))))

(defun decode-digits (digits)
  (let ((found-digits (make-array '(10) :initial-element nil)))
    (find-digit 1 (has-segments 2))
    (find-digit 7 (has-segments 3))
    (find-digit 4 (has-segments 4))
    (find-digit 8 (has-segments 7))
    (find-digit 3 (has-segments 5) (has-common-segments 2 1))
    (find-digit 5 (has-segments 5) (has-common-segments 3 4))
    (find-digit 2 (has-segments 5))
    (find-digit 0 (has-common-segments 4 5))
    (find-digit 9 (has-common-segments 3 7))
    (find-digit 6 t)
    (loop 
      for d across found-digits
      for i from 0
      collect (cons d i))))

(defun main ()
  (for/fold
    ((sum1 0)
     (sum2 0))
    ((entry (read-input-as-list 08 #'parse)))
    (destructuring-bind (digits displays) entry
      (let ((alist (decode-digits digits)))
        (values
          (+ sum1
             (for/count
               ((display displays))
               (member (length display) '(2 3 4 7))))
          (+ sum2
             (for/fold
               ((value 0))
               ((display displays))
               (+ (* 10 value) (cdr (assoc display alist :test #'equal))))))))
    :result (format t "~d~%~d~%" sum1 sum2)))

