(defpackage :e33
  (:use :cl))

(in-package :e33)

(defun is-curious (n d)
  (declare (fixnum n d))
  (let ((nd (/ n d))
        (rn (rem n 10))
        (fn (floor n 10))
        (rd (rem d 10))
        (fd (floor d 10)))
    (cond
      ((= 0 rn rd) nil)
      ((= rn fd) (and (/= 0 rd) (= nd (/ fn rd))))
      ((= rn rd) (and (/= 0 fd) (= nd (/ fn fd))))
      ((= fn fd) (and (/= 0 rd) (= nd (/ rn rd))))
      ((= fn rd) (and (/= 0 fd) (= nd (/ rn fd)))))))


(let ((result 1))
  (do ((n 10 (1+ n)))
      ((>= n 100))
    (do ((d (1+ n) (1+ d)))
        ((>= d 100))
      (when (is-curious n d)
        (format t "~A ~A~%" n d)
        (setf result (* result (/ n d))))))
  result)
