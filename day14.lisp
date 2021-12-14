(defpackage :day14
  (:use :cl :aoc-misc :cl-ppcre :forfuncs)
  (:export main)
  (:import-from :fset :empty-map :with :lookup :convert))

(in-package :day14)

(defun run-steps (n rules polymer)
  (if (zerop n)
    polymer
    (for/fold
      ((map (empty-map 0)))
      ((pair polymer))
      (destructuring-bind (name . count) pair
        (reduce
          (lambda (m p) (with m p (+ count (lookup m p))))
          (lookup rules name) :initial-value map))
      :result (run-steps (1- n) rules (convert 'list map)))))

(defun main ()
  (let*
    ((input (read-input-as-list 14))
     (template-letters (coerce (car input) 'list))
     (extremities (list (car template-letters) (car (last template-letters))))
     (pre-count
       (for/fold
         ((map (empty-map 0)))
         ((letter extremities))
         (with map letter (1+ (lookup map letter)))))
     (rules
       (for/fold 
         ((rules (empty-map)))
         ((line (cddr input)))
         (multiple-value-bind (_ regs) (scan-to-strings "^(.)(.) -> (.)$" line)
           (destructuring-bind (a b c) (map 'list (lambda (x) (char x 0)) regs)
             (with rules (list a b) (list (list a c) (list c b)))))))
     (template
       (for/fold
         ((map (empty-map 0)))
         ((a template-letters)
          (b (cdr template-letters)))
         (let ((pair (list a b)))
           (with map pair (1+ (lookup map pair))))
         :result (convert 'list map)))
     (first-polymer  (run-steps 10 rules template))
     (second-polymer (run-steps 30 rules first-polymer)))

    (dolist (polymer `(,first-polymer ,second-polymer))
      (for/fold
        ((map pre-count))
        ((pair polymer))
        (destructuring-bind (letters . count) pair
          (reduce
            (lambda (m l) (with m l (+ count (lookup m l))))
            letters :initial-value map))
        :result
        (let ((counts (sort (mapcar (lambda (x) (/ (cdr x) 2)) (convert 'list map)) '>)))
          (print (- (first counts) (car (last counts)))))))))

