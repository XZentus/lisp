(defpackage :e31
  (:use :cl))

(in-package :e31)

(defparameter *coins* '(1 2 5 10 20 50 100 200))

(defun count-ways (target &key (coins *coins*))
  (declare (type fixnum target))
  (labels ((routine (target coins)
             (declare (type fixnum target))
             (cond
               ((= target 0) 1)
               ((or (< target 0) (null coins) (> (car coins) target)) 0)
               (t (+ (routine (- target (car coins)) coins)
                     (routine target (cdr coins)))))))
    (routine target coins)))

;; (count-ways 200)
