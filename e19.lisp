(defpackage :e19
  (:use :cl))

(in-package :e19)

(defun leap-yearp (y)
  (cond
    ((= 0 (rem y 400)) t)
    ((= 0 (rem y 100)) nil)
    ((= 0 (rem y 4)) t)))

(defun days-in-month (m y)
  (setf m (rem m 12))
  (cond
    ((= m 2) (if (leap-yearp y)
                 29
                 28))
    (t (aref #(0 31 0 31 30 31 30 31 31 30 31 30 31) m))))
