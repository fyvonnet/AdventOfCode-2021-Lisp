(defpackage :day21
  (:use :cl :aoc-misc :forfuncs)
  (:import-from :serapeum :nlet)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day21)

(defparameter *re* "Player \\d starting position: (\\d+)")
(defvar throws-points nil)
(defvar cache (make-hash-table :test #'equal))

(defun parse (line)
  (multiple-value-bind (_ regs) (scan-to-strings *re* line)
    (list 0 (1- (parse-integer (aref regs 0))))))

(defun play (players &optional (dice 0) (turns 0))
  (destructuring-bind ((score position) b) players
    (multiple-value-bind (points new-dice)
      (nlet rec ((c 3) (pts 0) (d dice))
        (if (zerop c)
          (values pts d)
          (rec
            (1- c)
            (+ pts (1+ d))
            (mod (1+ d) 100))))
      (let*
        ((new-position (mod (+ points position) 10))
         (new-score (+ score (1+ new-position))))
        (if (>= new-score 1000)
          (* (1+ turns) 3 (first b))
          (play (list b (list new-score new-position)) new-dice (1+ turns)))))))

(defun get-count-wins (a-score a-pos b-score b-pos throw a-turn)
  (let ((cached-wins (gethash (list a-score a-pos b-score b-pos throw a-turn) cache)))
    (if cached-wins
      cached-wins
      (let*
        ((new-pos
           (if a-turn
             (mod (+ a-pos throw) 10)
             (mod (+ b-pos throw) 10)))
         (new-score
           (if a-turn
             (+ a-score (1+ new-pos))
             (+ b-score (1+ new-pos)))))
        (cond
          ((>= new-score 21)
           (let ((lst (if a-turn '(1 0) '(0 1))))
             (setf (gethash (list a-score a-pos b-score b-pos throw a-turn) cache) lst) lst))
          (t
            (let*
              ((func
                 (if a-turn
                   (lambda (th) (get-count-wins new-score new-pos b-score b-pos th nil))
                   (lambda (th) (get-count-wins a-score a-pos new-score new-pos th t))))
               (wins
                 (reduce
                   (lambda (x y) (mapcar #'+ x y))
                   (mapcar func throws-points))))
              (setf (gethash (list a-score a-pos b-score b-pos throw a-turn) cache) wins)
              wins)))))))

(defun main ()
  (let ((players (read-input-as-list 21 #'parse)))
    (print (play players))

    (for*
      ((a '(3 2 1))
       (b '(3 2 1))
       (c '(3 2 1)))
      (push (+ a b c) throws-points))

    (destructuring-bind ((_ pos-a) (_ pos-b)) players
      (print
        (apply
          #'max 
          (reduce
            (lambda (x y) (mapcar #'+ x y))
            (mapcar
              (lambda (th) (get-count-wins 0 pos-a 0 pos-b th t))
              throws-points)))))))

