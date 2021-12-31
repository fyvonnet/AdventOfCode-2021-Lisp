(defpackage :day04
  (:use :cl :aoc-misc :iterate)
  (:export main)
  (:import-from :cl-ppcre :split)
  (:import-from :forfuncs :for/and :for/sum)
  (:import-from :trivia :match))

(in-package :day04)

(defvar boards       )
(defvar locations    )
(defvar still-playing)

(defun decode (line re)
  (unless (zerop (length line))
    (mapcar #'parse-integer (split re (string-trim '(#\Space) line)))))

(defun make-boards (lines)
  (unless (null lines)
    (destructuring-bind (_ a b c d e &rest rest) lines
      (cons
        (mapcar (lambda (l) (decode l "\\s+")) (list a b c d e))
        (make-boards rest)))))

(defun sum-winning-board (coord)
  (destructuring-bind (b _ _) coord
    (iterate outer
      (for y below 5)
      (iterate
          (for x below 5)
          (for v = (aref boards b y x))
          (when v (in outer (summing v)))))))

(defun aref-boards (boards coord)
  (apply #'aref (cons boards coord)))

(defun (setf aref-boards) (value boards coord)
  (setf (apply #'aref (cons boards coord)) value))

(defun mark-number (winners number &optional (coords (aref locations number)))
  (if (null coords)
    winners
    (destructuring-bind (first-coord &rest rest-coords) coords
      (setf (aref-boards boards first-coord) nil)
      (mark-number
        (if 
          (destructuring-bind (b y x) first-coord
            (and
              (aref still-playing b)
              (or
                (for/and ((i '(0 1 2 3 4))) (null (aref boards b i x)))
                (for/and ((i '(0 1 2 3 4))) (null (aref boards b y i))))))
          (progn
            (setf (aref still-playing (car first-coord)) nil)
            (cons (* number (sum-winning-board first-coord)) winners))
          winners)
        number
        rest-coords))))

(defun main ()
  (let*
    ((input        (read-input-as-list 4   ))
     (numbers      (decode (car input) "," ))
     (boards-list  (make-boards (cdr input)))
     (boards-count (length boards-list     )))

    (setf boards        (make-array  (list boards-count 5 5) :initial-contents boards-list))
    (setf locations     (make-array  (list (length numbers)) :initial-element  nil        ))
    (setf still-playing (make-array `(,boards-count)         :initial-element  t          ))

    (loop for b below (length boards-list) doing
          (loop for y below 5 doing
                (loop for x below 5 doing
                      (push `(,b ,y ,x) (aref locations (aref boards b y x))))))

    (let ((winners (reduce #'mark-number numbers :initial-value nil)))
      (format t "~a~%~a~%" (car (last winners)) (car winners)))))

