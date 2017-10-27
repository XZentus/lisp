(do* ((x 1)
      (y 2)
      (s 0))
     ((>= x 4000000) (print s))
  (when (evenp x) (incf s x))
  (let ((n (+ x y)))
    (setf x y
	  y n)))
