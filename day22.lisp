(defpackage :day22
  (:use :cl :aoc-misc :cl-ppcre :forfuncs)
  (:import-from :fset :convert :lookup :empty-map :size :with)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day22)

(defun parse (line)
  (destructuring-bind (onoff rest) (split " " line)
    (cons
      (string= onoff "on")
      (destructuring-bind (xmin xmax ymin ymax zmin zmax)
        (apply
          #'append
          (mapcar
            (lambda (str)
              (multiple-value-bind (_ regs) (scan-to-strings ".=(-?\\d+)\.\.(-?\\d+)" str)
                (map 'list #'parse-integer regs)))
            (split "," rest)))
        (list xmin (1+ xmax) ymin (1+ ymax) zmin (1+ zmax))))))

(defun main ()
  (let
    ((input (read-input-as-list 22 #'parse))
     (array (make-array '(101 101 101) :initial-element nil))
     (cubes 0))

    (loop for step in input doing
      (destructuring-bind (turn-on xa xb ya yb za zb) step
        (let
          ((xmin (max -50 xa))
           (xmax (min  50 xb))
           (ymin (max -50 ya))
           (ymax (min  50 yb))
           (zmin (max -50 za))
           (zmax (min  50 zb)))
          (loop for z from zmin below zmax doing
            (loop for y from ymin below ymax doing
              (loop for x from xmin below xmax doing
                (let*
                  ((xx (+ 50 x))
                   (yy (+ 50 y))
                   (zz (+ 50 z))
                   (is-on (aref array zz yy xx)))
                  (cond
                    ((and      turn-on  (not is-on)) (incf cubes))
                    ((and (not turn-on)      is-on ) (decf cubes)))
                  (setf (aref array zz yy xx) turn-on))))))))

    (format t "~d~%" cubes)

    (for/fold
      ((xs nil) (ys nil) (zs nil))
      ((step input))
      (destructuring-bind (_ xmin xmax ymin ymax zmin zmax) step
        (values
          (cons xmin (cons xmax xs))
          (cons ymin (cons ymax ys))
          (cons zmin (cons zmax zs))))
      :result
      (destructuring-bind (xs-sorted ys-sorted zs-sorted)
        (mapcar 
          (lambda (lst) (remove-duplicates (sort lst #'<)))
          (list xs ys zs))
        (destructuring-bind (map-xs map-ys map-zs)
          (mapcar
            (lambda (lst)
              (for/fold
                ((map (empty-map)))
                ((a lst)
                 (i (loop for x below (length lst) collect x)))
                (with map a i)))
            (list xs-sorted ys-sorted zs-sorted))
          (destructuring-bind (dists-xs dists-ys dists-zs)
            (mapcar
              (lambda (lst)
                (nlet rec ((lst lst) (dists nil))
                  (if (null (cdr lst))
                    (coerce (reverse dists) 'vector)
                    (rec (cdr lst) (cons (- (cadr lst) (car lst)) dists)))))
              (list xs-sorted ys-sorted zs-sorted))
            (for/fold
              ((new-input nil))
              ((step input))
              (destructuring-bind (turn-on &rest limits) step
                (cons
                  (cons
                    (if turn-on 1 0)
                    (mapcar
                      #'lookup
                      (list map-xs map-xs map-ys map-ys map-zs map-zs)
                      limits))
                  new-input))
              :result
              (let*
                ((nxs (length xs-sorted))
                 (nys (length ys-sorted))
                 (nzs (length zs-sorted))
                 (array (make-array `(,nzs ,nys ,nxs) :element-type 'bit :initial-element 0)))
                (loop for step in (reverse new-input) doing
                  (destructuring-bind (turn-on xmin xmax ymin ymax zmin zmax) step
                    (loop for x from xmin below xmax doing
                      (loop for y from ymin below ymax doing
                        (loop for z from zmin below zmax doing
                          (setf (aref array z y x) turn-on))))))
                (setf cubes 0)
                (loop for x below nxs doing
                  (loop for y below nys doing
                    (loop for z below nzs doing
                      (when (= 1 (aref array z y x))
                        (incf cubes (* (aref dists-zs z) (aref dists-ys y) (aref dists-xs x)))))))
                (format t "~D~%" cubes)))))))))

