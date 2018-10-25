(defpackage :e19
  (:use :cl :iterate))

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

(defun next-month-starting-day (d m)
  (rem (+ d m) 7))

;; (let ((d 1)
;;       (mon1 0))
;;   (iter (for y from 1901 to 2000)
;;     (iter (for m from 1 to 12)
;;       (when (= d 6)
;;         (format t "~A ~A ~A~%" y m d)
;;         (incf mon1))
;;       (setf d (next-month-starting-day d (days-in-month m y)))))
;;   (values d mon1))
