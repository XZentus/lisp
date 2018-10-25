(defpackage :e23
  (:use :cl))

(in-package :e23)

(defun divs-sum (n)
  (declare (fixnum n))
  (let ((result 1))
    (do ((x 2 (1+ x)))
        ((> x (floor n 2)))
      (when (zerop (rem n x))
        (incf result x)))
    result))

(defparameter *abundants*
  (let ((result ()))
    (do ((x 12 (1+ x)))
        ((>= x 28124))
      (when (> (divs-sum x) x)
        (push x result)))
    result))

(let ((result (make-array '(28125) :element-type 'fixnum :initial-element 0))
      (sum 0))
  (do ((i1 *abundants* (cdr i1)))
      ((null i1))
    (do ((i2 i1 (cdr i2)))
        ((null i2))
      (let ((i (+ (car i1) (car i2))))
        (when (< i 28125)
          (setf (aref result i) 1)))))
  (loop for i from 0
        for e across result
        do (when (= e 0)
;;             (print i)
             (incf sum i)))
;;  (print result)
  (print sum)
  sum)
