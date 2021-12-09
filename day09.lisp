(defpackage :day09
  (:use :cl :aoc-misc :aoc-coord :functional-queue)
  (:export main)
  (:import-from :alexandria :copy-array)
  (:import-from :serapeum :nlet))

(in-package :day09)

(defun explore-map (map size queue)
  (if (queue-empty-p queue)
    size
    (let ((coord (queue-head queue)))
      (explore-map
        map
        (1+ size)
        (reduce
          (lambda (q d)
            (let*
              ((neighbour (next-coord d coord))
               (nsquare (aref-coord-checked map neighbour)))
              (if (or (null nsquare) (= 9 nsquare))
                q
                (progn
                  (setf (aref-coord map neighbour) nil)
                  (queue-snoc q neighbour))))) 
          *all-absolute-dirs* :initial-value (queue-tail queue))))))

(defun main ()
  (let
    ((input (read-input-as-array 09 (lambda (c) (parse-integer (string c)))))
     (low-points '()))
    (destructuring-bind (height width) (array-dimensions input)
      (loop :for y :below height :do
        (loop :for x :below width :do
          (let*
            ((coord (make-coord x y))
             (square (aref-coord input coord)))
            (when
              (nlet rec ((dirs *all-absolute-dirs*))
                (if (null dirs)
                  t
                  (let*
                    ((neighbour (next-coord (car dirs) coord))
                     (nsquare (aref-coord-checked input neighbour)))
                    (cond 
                      ((null nsquare) (rec (cdr dirs)))
                      ((< square nsquare) (rec (cdr dirs)))
                      (t nil)))))
              (push coord low-points))))))

    (print (loop :for lp :in low-points :sum (1+ (aref-coord input lp))))

    (print
      (destructuring-bind (a b c &rest _)
        (sort
          (loop :for lp :in low-points :collect
            (let ((input-copy (copy-array input)))
              (setf (aref-coord input-copy lp) nil)
              (explore-map input-copy 0 (queue-snoc (empty-queue) lp))))
          '>)
        (* a b c)))))

