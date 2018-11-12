(defpackage :e50
  (:use :cl))

(in-package :e50)

(defparameter *limit* 1000001)

(defparameter *erast-primes*
  (let ((result (make-array (list (1+ *limit*)) :element-type 'bit :initial-element 1)))
    (do ((i 2 (1+ i)))
        ((>= (* i i) *limit*))
      (do ((composite (* i i) (+ composite i)))
          ((>= composite *limit*))
        (setf (aref result composite) 0)))
    result))

(defun next-prime (start)
  (cond
    ((>= start *limit*)
     (+ *limit* 100500))
    ((evenp start) (next-prime (1+ start)))
    (t
     (do ((i start (+ i 2)))
         ((or (>= i *limit*)
              (/= (aref *erast-primes* i) 0))
          i)))))

(defun get-cons-sum (start)
  (let ((n start)
        (count 1)
        (sum start)
        (nextcount 1)
        (nextn start)
        (nextsum start))
    (tagbody
     step
       (setf nextn (next-prime (1+ nextn))
             nextsum (+ nextsum nextn)
             nextcount (1+ nextcount))
;;       (format t "nextn=~A nexsum=~A nextcount=~A~%" nextn nextsum nextcount)
       (when (or (>= nextsum *limit*)
                 (>= nextn *limit*))
         (go end))
       (when (/= 0 (aref *erast-primes* nextsum))
         (setf count nextcount
               sum nextsum
               n nextn))
;;       (format t "n=~A sum=~A count=~A~%" n sum count)
       (go step)
     end
;;       (format t "~A ~A ~A~%" count sum n)
       )
    (list start count sum n)))

(defun routine ()
  (let ((i 2)
        (maxi 0)
        (maxc 0)
        (maxs 0)
        (maxn 0))
    (tagbody
     step
       (destructuring-bind (st c sm n)
           (get-cons-sum i)
         (when (> c maxc)
           (setf maxi st
                 maxc c
                 maxs sm
                 maxn n)))
       (incf i)
       (when (< i *limit*)
         (go step)))
    (format t "I = ~A COUNT = ~A SUM = ~A N = ~A~%" maxi maxc maxs maxn)
    (values maxi maxc maxs maxn)))
