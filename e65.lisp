(defun sq-converg (n)
  (labels ((routine (n)
             (if (<= n 1)
                 1/2
                 (/ 1 (+ 2 (routine (1- n)))))))
    (1+ (routine n))))

(defun sum-of-digits (n)
  (let ((result 0))
    (tagbody
     step
       (incf result (rem n 10))
       (setf n (floor n 10))
       (when (> n 0)
         (go step)))
    result))


(defun e-converg (n)
  (labels ((routine (n lim)
             (cond
               ((>= n lim) 0)
               ((= 1 (rem n 3))
                (let ((c (* 2 (/ (+ 2 n) 3))))
;;                  (format t "~A => ~A~%" n c)
                  (/ 1 (+ c (routine (1+ n) lim)))))
               (t (/ 1 (+ 1 (routine (1+ n) lim)))))))
    (+ 2 (routine 0 (1- n)))))

(sum-of-digits (numerator (e-converg 100)))
