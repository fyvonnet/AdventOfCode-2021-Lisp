(defpackage :day09
  (:use :cl :aoc-misc :aoc-coord :functional-queue)
  (:export main)
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
  (let ((low-points '()))
    (destructuring-bind (height width) (array-dimensions heightmap)
      (loop :for y :below height :do
        (loop :for x :below width :do
          (let*
            ((coord (make-coord x y))
             (square (aref-coord heightmap coord)))
            (when
              (nlet rec ((dirs *all-absolute-dirs*))
                (if (null dirs)
                  t
                  (let*
                    ((neighbour (next-coord (car dirs) coord))
                     (nsquare (aref-coord-checked heightmap neighbour)))
                    (cond 
                      ((null nsquare) (rec (cdr dirs)))
                      ((< square nsquare) (rec (cdr dirs)))
                      (t nil)))))
              (push coord low-points))))))
    (print (loop :for lp :in low-points :sum (1+ (aref-coord heightmap lp))))
    (destructuring-bind (a b c &rest _)
      (sort
        (loop :for lp :in low-points :collect
          (progn
            (setf (aref-coord heightmap lp) nil)
            (explore-map 0 (queue-snoc (empty-queue) lp))))
        '>)
      (print (* a b c)))))

