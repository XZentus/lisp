(let ((result 0))
           (loop for x from 1 below 20 do
                 (loop for y from 1 below 25 do
                       (when ;(= y (ceiling (log (expt x y) 10)))
                           (= y (length (format nil "~A" (expt x y))))
                         (format t "~A^~A = ~A~%" x y (expt x y))
                         (incf result))))
           result)
