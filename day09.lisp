(defpackage :day09
  (:use :cl :aoc-misc :aoc-coord :functional-queue)
  (:export main)
  (:import-from :forfuncs :for/and :for/fold :for/sum))

(in-package :day09)

(defvar heightmap)

(defun fill-queue (coord)
  (lambda (queue dir)
    (let*
      ((neighbour (next-coord dir coord)))
      (if (= 9 (aref-coord-safe heightmap neighbour 9))
        queue
        (progn
          (setf (aref-coord heightmap neighbour) 9)
          (queue-snoc queue neighbour))))) )

(defun measure-basin (lp &optional (size 0) (queue (queue-snoc (empty-queue) lp)))
  (if (queue-empty-p queue)
    (1- size)
    (measure-basin
      nil
      (1+ size)
      (reduce
        (fill-queue (queue-head queue))
        *all-absolute-dirs*
        :initial-value (queue-tail queue)))))


(defun main ()
  (setf heightmap (read-input-as-array 09 (lambda (c) (parse-integer (string c)))))

  (let
    ((low-points
       (for/fold
         ((lp '()))
         ((coord (all-matrix-coords heightmap)))
         (if
           (for/and
             ((dir *all-absolute-dirs*))
             (< 
               (aref-coord heightmap coord)
               (aref-coord-safe heightmap (next-coord dir coord) 9)))
           (cons coord lp)
           lp))))

    (print (for/sum ((lp low-points)) (1+ (aref-coord heightmap lp))))

    (destructuring-bind (a b c &rest _)
      (sort (mapcar #'measure-basin low-points) '>)
      (print (* a b c)))))

