(defpackage :day07
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split))

(in-package :day07)

(defun main ()
  (let*
    ((positions (mapcar #'parse-integer (split "," (car (read-input-as-list 07)))))
     (max-pos (loop :for p :in positions :maximize p)))
    (dolist (func (list #'identity (lambda (n) (/ (* (1+ n) n) 2))))
      (print
        (loop :for new-pos :to max-pos
              :minimize (loop :for p :in positions
                              :sum (funcall func (abs (- new-pos p)))))))))

