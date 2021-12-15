(defpackage :day15
  (:use :cl :aoc-misc :aoc-coord :forfuncs :leftist-heap :trivia)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day15)

(defun compare (a b)
  (< (cadr a) (cadr b)))

(defun safe-navigation (map)
  (let
    ((init-heap (leftist-insert (list (make-coord 0 0) 0) nil #'compare))
     (destination
       (destructuring-bind (height width) (array-dimensions map)
         (make-coord (1- width) (1- height)))))
    (nlet rec ((heap init-heap))
      (destructuring-bind (coord level) (leftist-find-min heap) 
        (if (coord= coord destination)
          level
          (for/fold
            ((heap (leftist-delete-min heap #'compare)))
            ((dir *all-absolute-dirs*))
            (let ((neighbour (next-coord dir coord)))
              (match (aref-coord-safe map neighbour)
                (nil heap)
                (n (progn
                     (setf (aref-coord map neighbour) nil)
                     (leftist-insert (list neighbour (+ n level)) heap #'compare)))))
            :result (rec heap)))))))

(defun main ()
  (let ((map (read-input-as-array 15 (lambda (c) (parse-integer (string c))))))
    (destructuring-bind (height width) (array-dimensions map)
      (let ((extended-map (make-array (list (* 5 height) (* 5 width)))))

        (loop for yy below 5 doing
          (loop for xx below 5 doing
            (loop for y below height doing
              (loop for x below width doing
                (setf
                  (aref extended-map (+ y (* yy height)) (+ x (* xx width)))
                  (1+ (mod (+ yy xx (1- (aref map y x))) 9)) )))))

        (dolist (m `(,map ,extended-map))
          (print (safe-navigation m)))))))

