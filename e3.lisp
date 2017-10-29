(defparameter *primes*
  (let* ((raw-data '(2 3 5 7 11))
	 (l (length raw-data)))
    (make-array (list l) :initial-contents raw-data :adjustable t :fill-pointer t)))

(defparameter *limit* 600851475143)

(defparameter *sqlim* (ceiling (sqrt *limit*)))

(do ((x (aref *primes* (1- (length *primes*))) (+ x 2)))
    ((> x *sqlim*))
  (unless
      (let ((tmp-lim (ceiling (sqrt x))))
	(loop for d across *primes*
	   when (> d tmp-lim) return nil
	   when (zerop (mod x d)) return t))
    (vector-push-extend x *primes*)))

(do ((i (1- (length *primes*)) (1- i)))
    ((or (< i 0)
	 (zerop (mod *limit* (aref *primes* i))))
     (print (if (< i 0)
		'ERROR
		(aref *primes* i)))))

