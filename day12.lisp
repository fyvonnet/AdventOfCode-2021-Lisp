(defpackage :day12
  (:use :cl :aoc-misc :cl-ppcre :forfuncs :trivia)
  (:export main)
  (:import-from :fset :empty-map :lookup :with :empty-set :convert))

(in-package :day12)

(defvar cave-system)

(defun decode-string (str)
  (cond
    ((string= "start" str) :start)
    ((string= "end" str) :end)
    ((upper-case-p (char str 0)) (cons :big str))
    (t (cons :small str))))

(defun decode (line)
  (mapcar #'decode-string (split "-" line)))

(defun collect-paths (stack &optional (set (empty-set)))
  (if (null stack)
    set
    (destructuring-bind (cave path allowed-visits) (car stack)
      (if (eq :end cave)
        (collect-paths (cdr stack) (with set (cdr path)))
        (for/fold
          ((new-stack (cdr stack)))
          ((next-cave
             (remove-if
               (lambda (c) (let ((r (lookup allowed-visits c))) (and r (zerop r))))
               (lookup cave-system cave))))
          (cons
            (list
              next-cave
              (cons next-cave path)
              (match (lookup allowed-visits next-cave)
                (nil allowed-visits)
                (n (with allowed-visits next-cave (1- n)))))
            new-stack)
          :result (collect-paths new-stack set))))))

(defun main ()
  (let ((input (read-input-as-list 12 #'decode)))
    (setf
      cave-system
      (for/fold
        ((cave-system (empty-map)))
        ((path (append input (mapcar #'reverse input))))
        (destructuring-bind (begin end) path
          (if (or (eq begin :end) (eq end :start))
            cave-system
            (let ((content (lookup cave-system begin)))
              (with cave-system begin (cons end content))))))))
  (let*
    ((small-caves
       (remove-if-not
         (lambda (x) (and (listp x) (eq :SMALL (first x))))
         (mapcar #'first (convert 'list cave-system))))
     (allowed-visits (convert 'fset:map (mapcar (lambda (x) (cons x 1)) small-caves))))

    (print (fset:size (collect-paths (list (list :start nil allowed-visits)))))

    (for/fold
      ((set (empty-set)))
      ((small-cave small-caves))
      (collect-paths (list (list :start nil (with allowed-visits small-cave 2))) set)
      :result (print (fset:size set)))))

