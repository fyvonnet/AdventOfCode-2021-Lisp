(defpackage :day09
  (:use :cl :aoc-misc :aoc-coord :functional-queue)
  (:export main)
  (:import-from :forfuncs :for/and :for/fold :for/sum)
  (:import-from :serapeum :nlet))

(in-package :day09)

(defvar heightmap)

(defun explore-map (size queue)
  (if (queue-empty-p queue)
    size
    (let ((coord (queue-head queue)))
      (explore-map
        (1+ size)
        (reduce
          (lambda (q d)
            (let*
              ((neighbour (next-coord d coord))
               (nsquare (aref-coord-checked heightmap neighbour)))
              (if (or (null nsquare) (= 9 nsquare))
                q
                (progn
                  (setf (aref-coord heightmap neighbour) nil)
                  (queue-snoc q neighbour))))) 
          *all-absolute-dirs* :initial-value (queue-tail queue))))))


(defun main ()
  (setf heightmap (read-input-as-array 09 (lambda (c) (parse-integer (string c)))))

  (let
    ((low-points
       (for/fold
         ((lp '())) nil
         ((coord
            (destructuring-bind (height width) (array-dimensions heightmap)
              (loop :for y :below height :append
                (loop :for x :below width :collect (make-coord x y))))))
         (let ((square (aref-coord heightmap coord)))
           (if
             (for/and
               ((dir *all-absolute-dirs*))
               (let*
                 ((neighbour (next-coord dir coord))
                  (nsquare (aref-coord-checked heightmap neighbour)))
                 (or (null nsquare) (> nsquare square))))
             (cons coord lp)
             lp)))))

    (print (for/sum ((lp low-points)) (1+ (aref-coord heightmap lp))))

    (destructuring-bind (a b c &rest _)
      (sort
        (mapcar
          (lambda (lp)
            (progn
              (setf (aref-coord heightmap lp) nil)
              (explore-map 0 (queue-snoc (empty-queue) lp))))
          low-points)
        '>)
      (print (* a b c)))))

