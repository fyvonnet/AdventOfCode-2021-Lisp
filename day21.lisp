(defpackage :day21
  (:use :cl :aoc-misc)
  (:import-from :serapeum :nlet)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day21)

(defparameter *re* "Player \\d starting position: (\\d+)")

(defun play (players &optional (dice 0) (turns 0))
  (destructuring-bind ((score position) b) players
    (multiple-value-bind (points new-dice)
      (nlet rec ((c 3) (pts 0) (d dice))
        (if (zerop c)
          (values pts d)
          (rec
            (1- c)
            (+ pts (1+ d))
            (mod (1+ d) 100))))
      (let*
        ((new-position (mod (+ points position) 10))
         (new-score (+ score (1+ new-position))))
        (if (>= new-score 1000)
          (* (* (1+ turns) 3) (first b))
          (play (list b (list new-score new-position)) new-dice (1+ turns)))))))

(defun parse (line)
  (multiple-value-bind (_ regs) (scan-to-strings *re* line)
    (list 0 (1- (parse-integer (aref regs 0))))))

(defun main ()
  (let ((players (read-input-as-list 21 #'parse)))
    (format t "~D~%" (play players))))

