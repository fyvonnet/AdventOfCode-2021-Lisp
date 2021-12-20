(defpackage :day20
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day20)


(defun display-image (image)
  (destructuring-bind (height width) (array-dimensions image)
    (loop for y below height doing
      (loop for x below width doing
        (format t "~a" (aref image y x)))
      (format t "~%"))
    (format t "~%")))

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
         (width (+ 2 in-width))
         (image (make-array (list height width))))
        (loop for y below height doing
          (loop for x below width doing
            (let
              ((in-x (- x 2))
               (in-y (- y 2))
               (i 0))
              (loop
                with bit-value = 1
                with sum = 0
                for ny from 2 downto 0 doing
                (loop for nx from 2 downto 0 doing
                  (progn
                    (when (char= (aref-safe in-image (+ in-y ny) (+ in-x nx) default) #\#)
                      (incf i bit-value))
                    (setf bit-value (* 2 bit-value)))))
              (setf (aref image y x) (aref algorithm i)))))
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
      (let ((output-image (enhance-image algorithm input-image n)))
        (destructuring-bind (height width) (array-dimensions output-image)
          (loop
            with count = 0
            for y below height doing
            (loop for x below width doing
              (when (char= #\# (aref output-image y x)) (incf count)))
            finally (print count)))))))

