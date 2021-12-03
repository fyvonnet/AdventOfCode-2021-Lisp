(defpackage :day03
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day03)

(defun decode (line)
  (map 'list (lambda (c) (if (char= c #\0) 0 1)) line))

(defun count-bits (numbers &optional new-numbers (counters '(0 . 0)))
  (destructuring-bind (zeros . ones) counters
    (if (null numbers)
      (list new-numbers (if (< ones zeros) 0 1))
      (let ((n (car numbers)))
        (count-bits
          (cdr numbers)
          (cons (cdr n) new-numbers)
          (if (zerop (car n))
            (cons (1+ zeros) ones)
            (cons zeros (1+ ones))))))))

(defun gamma-rate (numbers &optional bits)
  (if (null (car numbers))
    (reverse bits)
    (destructuring-bind (new-numbers bit) (count-bits numbers)
      (gamma-rate new-numbers (cons bit bits)))))

(defun bin-to-dec (bits &optional (n 0))
  (if (null bits)
    n
    (bin-to-dec (cdr bits) (+ (car bits) (* 2 n)))))

(defun main ()
  (let*
    ((input (read-input-as-list 3 #'decode))
     (gamma (gamma-rate input)))
    (print
      (* 
        (bin-to-dec gamma)
        (bin-to-dec (mapcar (lambda (b) (if (zerop b) 1 0)) gamma))))))

