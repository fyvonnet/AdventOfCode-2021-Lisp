(defpackage :day11
  (:use :cl :aoc-misc :aoc-coord :functional-queue)
  (:export main)
  (:import-from :forfuncs :for/and :for/fold)
  (:import-from :trivia :match))

(in-package :day11)

(defvar neighbours
  (mapcar
    (lambda (c) (destructuring-bind (x y) c (make-coord x y)))
    '((-1 -1) ( 0 -1) ( 1 -1)
      (-1  0)         ( 1  0)
      (-1  1) ( 0  1) ( 1  1))))

(defvar octopuses (read-input-as-array 11 (lambda (c) (parse-integer (string c)))))
(defvar all-octopuses-coords (all-matrix-coords octopuses))

(defun propagate-flash (queue count)
  (if (queue-empty-p queue)
    count 
    (for/fold
      ((q (queue-tail queue)))
      ((n neighbours))
      (let ((ncoord (coord+ n (queue-head queue))))
        (match (aref-coord-safe octopuses ncoord 0)
          (9
           (progn
             (setf (aref-coord octopuses ncoord) 0)
             (queue-snoc q ncoord)))
          (0 q)
          (n (progn (setf (aref-coord octopuses ncoord) (1+ n)) q))))
      :result (propagate-flash q (1+ count)))))

(defun run-steps (n &optional (count 0))
  (if (zerop n)
    count
    (let*
      ((queue
         (for/fold
           ((queue (empty-queue)))
           ((coord all-octopuses-coords))
           (match (aref-coord octopuses coord)
             (9
              (progn
                (setf (aref-coord octopuses coord) 0)
                (queue-snoc queue coord)))
             (_ (progn (incf (aref-coord octopuses coord)) queue)))))
       (new-count (propagate-flash queue count)))
      (run-steps (1- n) new-count))))

(defun find-simultaneous (&optional (n 100))
  (if 
    (for/and
      ((coord all-octopuses-coords))
      (zerop (aref-coord octopuses coord)))
    n
    (progn
      (run-steps 1)
      (find-simultaneous (1+ n)))))

(defun main ()
  (format t "~d~%~d~%" (run-steps 100) (find-simultaneous)))

