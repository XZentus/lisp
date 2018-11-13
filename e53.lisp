(defun fact (n)
  (let ((result 1))
    (do ((i 2 (1+ i)))
        ((> i n))
      (setf result (* result i)))
    result))

(defun c (n r)
  (/ (fact n) (* (fact r) (fact (- n r)))))


(let ((result 0))
  (do ((n 1 (1+ n)))
      ((> n 100))
    (do ((r 1 (1+ r)))
        ((> r n))
      (when (> (c n r) 1000000)
        (incf result))))
  result)
