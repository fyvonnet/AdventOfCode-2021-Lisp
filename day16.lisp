(defpackage :day16
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day16)

(defun read-hex-bin (stream)
  (match (read-line stream nil)
    (nil nil)
    (line
      (multiple-value-bind (_ regs) (scan-to-strings "^(.) = (....)$" line)
        (cons
          (cons
            (char (aref regs 0) 0)
            (map 'list #'parse-digit (aref regs 1)))
          (read-hex-bin stream))))))

(defvar hex-bin
  (with-open-file (stream "hex-bin.txt")
    (read-hex-bin stream)))

(defun hex-to-bin (hex)
  (apply #'append (map 'list (lambda (c) (cdr (assoc c hex-bin))) hex)))

(defun bin-to-dec (bin &optional (value 0))
  (if (null bin)
    value
    (bin-to-dec (cdr bin) (+ (* 2 value) (car bin)))))

(defvar expression (hex-to-bin (car (read-input-as-list 16))))

(defvar sum-versions 0)

(defun receive (nbits)
  (if (= 1 nbits) (pop expression) (loop repeat nbits collect (pop expression))))

(defun evaluate-expression ()
  (let
    ((packet-version (bin-to-dec (receive 3)))
     (packet-type    (bin-to-dec (receive 3))))
    (incf sum-versions packet-version)
    (if (= packet-type 4)
      (nlet rec ((stop nil) (sub-packets nil))
        (if stop
          (bin-to-dec (apply #'append (reverse sub-packets)))
          (let ((is-last (zerop (receive 1))))
            (rec is-last (cons (receive 4) sub-packets)))))
      (let
        ((sub-packets
           (if (zerop (receive 1))
             (let ((sub-packets-len (bin-to-dec (receive 15))))
               (nlet rec ((bits-received 0) (sub-packets nil))
                 (if (= bits-received sub-packets-len)
                   (reverse sub-packets)
                   (let
                     ((prev-bin-len (length expression))
                      (sub-packet (evaluate-expression)))
                     (rec
                       (+ bits-received (- prev-bin-len (length expression)))
                       (cons sub-packet sub-packets))))))
             (let ((n-sub-packets (bin-to-dec (receive 11))))
               (nlet rec ((n 0) (sub-packets nil))
                 (if (= n n-sub-packets)
                   (reverse sub-packets)
                   (let
                     ((sub-packet (evaluate-expression)))
                     (rec (1+ n) (cons sub-packet sub-packets)))))))))
        (case packet-type
          (0 (reduce #'+   sub-packets))
          (1 (reduce #'*   sub-packets))
          (2 (reduce #'min sub-packets))
          (3 (reduce #'max sub-packets))
          (5 (if (> (first sub-packets) (second sub-packets)) 1 0))
          (6 (if (< (first sub-packets) (second sub-packets)) 1 0))
          (7 (if (= (first sub-packets) (second sub-packets)) 1 0)))))))

(defun main ()
  (let ((part2-answer (evaluate-expression)))
    (format t "~d~%~d~%" sum-versions part2-answer)))

