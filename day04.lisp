(defpackage :day04
  (:use :cl :aoc-misc :iterate)
  (:export main)
  (:import-from :cl-ppcre :split))

(in-package :day04)

(defvar boards)
(defvar marked)
(defvar locations)
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

(defun all-row-coords (coord)
  (destructuring-bind (b y _) coord
    (iterate
      (for x below 5)
      (collect `(,b ,y ,x)))))

(defun all-col-coords (coord)
  (destructuring-bind (b _ x) coord
    (iterate
      (for y below 5)
      (collect `(,b ,y ,x)))))

(defun sum-winning-board (coord)
  (destructuring-bind (b _ _) coord
    (reduce
      (lambda (s c) (if (aref-boards marked c) s (+ s (aref-boards boards c))))
      (iterate
        (for y below 5)
        (appending 
          (iterate
            (for x below 5)
            (collect `(,b ,y ,x)))))
      :initial-value 0)))

(defun aref-boards (boards coord)
  (apply #'aref (cons boards coord)))

(defun (setf aref-boards) (value boards coord)
  (setf (apply #'aref (cons boards coord)) value))

(defun check-winner (coords)
  (if (null coords)
    t
    (when (aref-boards marked (car coords))
      (check-winner (cdr coords)))))

(defun mark-number (winners number &optional (coords (aref locations number)))
  (if (null coords)
    winners
    (destructuring-bind (first-coord &rest rest-coords) coords
      (setf (aref-boards marked first-coord) t)
      (mark-number
        (if 
          (and
            (aref still-playing (car first-coord))
            (or
              (check-winner (all-row-coords first-coord))
              (check-winner (all-col-coords first-coord))))
          (progn
            (setf (aref still-playing (car first-coord)) nil)
            (cons (* number (sum-winning-board first-coord)) winners))
          winners)
        number
        rest-coords))))

(defun main ()
  (let*
    ((input (read-input-as-list 4))
     (numbers (decode (car input) ","))
     (boards-list (make-boards (cdr input)))
     (boards-count (length boards-list))
     (dimensions (list boards-count 5 5)))

    (setf boards        (make-array dimensions              :initial-contents boards-list))
    (setf marked        (make-array dimensions              :initial-element  nil        ))
    (setf locations     (make-array (list (length numbers)) :initial-element  nil        ))
    (setf still-playing (make-array `(,boards-count)        :initial-element  t          ))

    (iterate
      (for b below (length boards-list))
      (iterate
        (for y below 5)
        (iterate
          (for x below 5)
          (push `(,b ,y ,x) (aref locations (aref boards b y x))))))

    (let ((winners (reduce #'mark-number numbers :initial-value nil)))
      (format t "~a~%~a~%" (car (last winners)) (car winners)))))

