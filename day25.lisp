(defpackage :day25
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day25)

(defun parse (c)
  (case c
    (#\. nil)
    (#\> 'east)
    (#\v 'south)
    (otherwise (error (format nil "Unknown character: ~c" c)))))

(defun moves (state coords ncoords)
  (when coords
    (destructuring-bind (x y) (car coords)
      (destructuring-bind (nx ny) (car ncoords)
        (let ((c (aref state y x)))
          (setf (aref state y x) nil)
          (setf (aref state ny nx) c))))
    (moves state (cdr coords) (cdr ncoords))))

(defun run-step (state)
  (let ((has-moved nil))
    (destructuring-bind (height width) (array-dimensions state) 
      (let
        ((coords nil)
         (ncoords nil))
        (loop for y below height doing
          (loop for x below width doing
            (let ((c (aref state y x)))
              (when (eq c 'east)
                (let ((nx (mod (1+ x) width)))
                  (unless (aref state y nx)
                    (setf has-moved t)
                    (setf  coords (cons (list  x y)  coords))
                    (setf ncoords (cons (list nx y) ncoords))))))))
        (moves state coords ncoords))
      (let
        ((coords nil)
         (ncoords nil))
        (loop for y below height doing
          (loop for x below width doing
            (let ((c (aref state y x)))
              (when (eq c 'south)
                (let ((ny (mod (1+ y) height)))
                  (unless (aref state ny x)
                    (setf has-moved t)
                    (setf  coords (cons (list x  y)  coords))
                    (setf ncoords (cons (list x ny) ncoords))))))))
        (moves state coords ncoords))
      has-moved)))

(defun steps-until-locked (state &optional (n 1))
  (if (null (run-step state))
    n
    (steps-until-locked state (1+ n))))

(defun main ()
  (format t "~D~%" (steps-until-locked (read-input-as-array 25 #'parse))))

