(print
 (- (let ((s (loop for x upto 100 sum x)))
      (* s s))
    (loop for x upto 100 sum (* x x))))
