(defpackage :day04
  (:use :cl :aoc-misc :iterate)
  (:export main)
  (:import-from :cl-ppcre :split))

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
    (iterate
      (for y below 5)
      (sum
        (iterate
          (for x below 5)
          (let ((value (aref-boards boards `(,b ,y ,x))))
            (sum (if value value 0))))))))

(defun check-winner (coord switch)
  (destructuring-bind (b y x) coord
    (iterate
      (with coord = (if switch (list b y 0) (list b 0 x)))
      (repeat 5)
      (cond
        ((aref-boards boards coord) (leave nil))
        (switch (incf (third  coord)))
        (t (incf (second coord))))
      (finally (return t)))))

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
          (and
            (aref still-playing (car first-coord))
            (or
              (check-winner first-coord t  )
              (check-winner first-coord nil)))
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

    (iterate
      (for b below (length boards-list))
      (iterate
        (for y below 5)
        (iterate
          (for x below 5)
          (push `(,b ,y ,x) (aref locations (aref boards b y x))))))

    (let ((winners (reduce #'mark-number numbers :initial-value nil)))
      (format t "~a~%~a~%" (car (last winners)) (car winners)))))

