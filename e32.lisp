(defpackage :e32
  (:use :cl))

(in-package :e32)

(defun ndigits (&rest ns)
  (apply #'+ (mapcar (lambda (n) (1+ (floor (log n 10)))) ns)))

(defun digits-vector (n)
  (declare (fixnum n))
  (let ((result (make-array '(10) :element-type '(integer 0 256) :initial-element 0)))
    (do ((x n (floor x 10)))
        ((< x 1))
      (incf (aref result (rem x 10))))
    result))

(defun check-nums (pattern ndigits &rest xs)
  (and (= (apply #'ndigits xs) ndigits)
       (equalp pattern
               (apply #'map 'vector #'+ (mapcar #'digits-vector xs)))))


(let* ((result ())
       (pattern #(0 1 1 1 1 1 1 1 1 1))
       (ndigits (reduce #'+ pattern)))
  (do ((x 1 (1+ x)))
      ((>= x 10000))
    (declare (fixnum x))
    (do ((y (1+ x) (1+ y)))
        ((< ndigits (ndigits x y (* x y))))
      (declare (fixnum y))
      (let ((p (* x y)))
        (when (check-nums pattern ndigits x y p)
          (format t "~A * ~A = ~A~%" x y p)
          (unless (find p result)
            (push p result))))))
  (format t "~A~%~A~%" result (reduce #'+ result)))
