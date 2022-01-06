(defpackage :day20
  (:use :cl :aoc-misc :iterate :forfuncs)
  (:import-from :alexandria :iota)
  (:export main))

(in-package :day20)

(defun aref-safe (image y x default)
  (destructuring-bind (height width) (array-dimensions image)
    (if (or (< x 0) (< y 0) (>= x width) (>= y height))
      default
      (aref image y x))))

(defun enhance-image (algorithm in-image count &optional (default #\.))
  (if (= count 0)
    in-image
    (destructuring-bind (in-height in-width) (array-dimensions in-image)
      (let*
        ((height (+ 2 in-height))
         (width  (+ 2 in-width ))
         (image (make-array (list height width))))
        (for*
          ((y (iota height))
           (x (iota width )))
          (let ((i 0))
            (for*
              ((ny '(-1 0 1))
               (nx '(-1 0 1)))
              (setf i (* 2 i))
              (when (char= (aref-safe in-image (+ (1- y) ny) (+ (1- x) nx) default) #\#)
                (incf i)))
            (setf (aref image y x) (aref algorithm i))))
        (enhance-image algorithm image (1- count) (aref algorithm (if (char= default #\.) 0 511)))))))

(defun main ()
  (let*
    ((input (read-input-as-list 20))
     (algorithm (make-array '(512) :initial-contents (car input)))
     (raw-input-image (cddr input))
     (input-image
       (make-array
         (list
           (length raw-input-image)
           (length (first raw-input-image)))
         :initial-contents raw-input-image)))
    (dolist (n '(2 50))
      (print
        (let ((output-image (enhance-image algorithm input-image n)))
          (destructuring-bind (height width) (array-dimensions output-image)
            (iterate outer
              (for y below height)
              (iterate
                (for x below width)
                (in outer (counting (char= #\# (aref output-image y x))))))))))))

