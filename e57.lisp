(defun sq-converg (n)
  (labels ((routine (n)
             (if (<= n 1)
                 1/2
                 (/ 1 (+ 2 (routine (1- n)))))))
    (1+ (routine n))))


(let ((result 0))
  (loop for x below 1001 do
        (let ((p (sq-converg x)))
          (when (> (length (format nil "~A" (numerator p)))
                   (length (format nil "~A" (denominator p))))
            (incf result))))
  result)
