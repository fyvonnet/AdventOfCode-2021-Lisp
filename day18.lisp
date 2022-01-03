(defpackage :day18
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day18)

(defun parse (line)
  (loop with depth = 0
        for c across line
        when (char= c #\[) do (incf depth)
        when (char= c #\]) do (decf depth)
        when (digit-char-p c) collect (cons depth (parse-integer (string c)))))

(defun explode (sfnum &optional (output nil))
  (unless (= 1 (length sfnum))
    (destructuring-bind ((depth-a . val-a) (depth-b . val-b) &rest rest) sfnum
      (if (and (> depth-a 4) (= depth-a depth-b))
        (reverse
          (append
            (when rest
              (destructuring-bind ((depth-r . val-r) &rest rest-of-rest) rest
                (reverse (cons (cons depth-r (+ val-r val-b)) rest-of-rest))))
            (list (cons (1- depth-a) 0))
            (when output
              (destructuring-bind ((depth-r . val-l) &rest rest-of-output) output
                (cons (cons depth-r (+ val-l val-a)) rest-of-output)))))
        (explode (cdr sfnum) (cons (car sfnum) output))))))

(defun split (sfnum &optional (output nil))
  (unless (null sfnum)
    (destructuring-bind ((depth . val) &rest rest) sfnum
      (if (< val 10)
        (split (cdr sfnum) (cons (car sfnum) output))
        (reverse
          (append
            (reverse rest)
            (multiple-value-bind (q r) (floor val 2)
              (list (cons (1+ depth) (+ q r)) (cons (1+ depth) q)))
            output))))))

(defun reduce-sfnum (sfnum)
  (let ((new-sfnum sfnum) (has-reduced nil))
    (loop for e = (explode new-sfnum)
          until (null e)
          do (setf new-sfnum e) (setf has-reduced t))
    (let ((s (split new-sfnum)))
      (when s
        (setf new-sfnum s) (setf has-reduced t)))
    (if has-reduced
      (reduce-sfnum new-sfnum)
      new-sfnum)))

(defun add-sfnums (a b)
  (reduce-sfnum
    (mapcar
      (lambda (s) (cons (1+ (car s)) (cdr s)))
      (append a b))))

(defun magnitude-rec (sfnum)
  (if (< (length sfnum) 2)
    sfnum
    (destructuring-bind ((depth-a . val-a) (depth-b . val-b) &rest _) sfnum
      (if (= depth-a depth-b)
        (cons (cons (1- depth-a) (+ (* 3 val-a) (* 2 val-b))) (magnitude-rec (cddr sfnum)))
        (cons (car sfnum) (magnitude-rec (cddr sfnum)))))))

(defun magnitude (sfnum)
  (if (= 1 (length sfnum))
    (cdar sfnum)
    (magnitude (magnitude-rec sfnum))))

(defun find-max (sfnums &optional (current-max 0))
  (if (= 1 (length sfnums))
    current-max
    (destructuring-bind (a &rest rest) sfnums
      (find-max
        rest
        (reduce
          (lambda (cmax b)
            (max
              cmax
              (magnitude (add-sfnums a b))
              (magnitude (add-sfnums b a))))
          rest :initial-value current-max)))))

(defun main ()
  (let
    ((input (read-input-as-list 18 #'parse)))
    (format t "~D~%~D~%"
            (magnitude (reduce #'add-sfnums input))
            (find-max input))))

