(defpackage :e52
  (:use :cl))

(in-package :e52)

(defun digits-vector (n)
  (declare (fixnum n))
  (let ((result (make-array '(10) :element-type '(integer 0 256) :initial-element 0)))
    (do ((x n (floor x 10)))
        ((< x 1))
      (incf (aref result (rem x 10))))
    result))

(defun same-digits2 (n1 n2 &key n1v n2v)
  (declare (type fixnum n1 n2))
  (if (= n1 n2)
      t
      (let ((d (/ n1 n2)))
        (if (<= 0.1 d 10)
            (equalp (or n1v (digits-vector n1))
                    (or n2v (digits-vector n2)))
            nil))))

(defun same-digits6 (n1)
  (declare (type fixnum n1))
  (let ((ds (digits-vector n1)))
    (every (lambda (n2) (same-digits2 n1 n2 :n1v ds)) (mapcar (lambda (m) (* n1 m)) '(2 3 4 5 6)))))


;; (do ((x 100000 (1+ x)))
;;     ((>= x 170000) nil)
;;   (if (same-digits6 x)
;; (return x)))
