(defpackage :e26
  (:use :cl)
  (:export :divide))

(in-package :e26)

(defun divide (del div)
  (let (result)
    (block routine
      (do ((i 0 (1+ i)))
          ((or (>= i 10000) (= 0 del)))
        (when (< del div)
          (setf del (* 10 del)))
        (loop for (d . j) in result do
              (when (= d del)
                (return-from routine (- i j))))
        (push (cons del i) result)
        (multiple-value-bind (res rest)
            (floor del div)
          (setf del rest)))
      -1)))


(do* ((x 2 (1+ x))
      (len 0 (divide 1 x))
      (maxlen 0)
      (maxx 2))
     ((>= x 1000) (cons maxx maxlen))
  (when (> len maxlen)
    (format t "~A ~A~%" x len)
    (setf maxlen len
          maxx x)))
  
