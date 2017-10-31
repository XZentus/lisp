(defparameter *limit* 20)

(defparameter *primes* #(( 2 . 0)
			 ( 3 . 0)
			 ( 5 . 0)
			 ( 7 . 0)
			 (11 . 0)
			 (13 . 0)
			 (17 . 0)
			 (19 . 0)))

(defparameter *primes-length* (length *primes*))

(defun count-max-divs (d)
  (let ((n 0))
    (loop for x from 2 upto *limit* do
	 (setf n (max n
		      (do ((value x (/ value d))
			   (current-n -1 (1+ current-n)))
			  ((not (integerp value)) current-n)))))
    n))

(do ((i 0 (1+ i)))
    ((>= i *primes-length*))
  (let ((old-val (cdr (aref *primes* i)))
	(new-val (count-max-divs (car (aref *primes* i)))))
    (when (> new-val old-val)
      (setf (cdr (aref *primes* i)) new-val))))

(print (let ((result 1))
	 (loop for x across *primes* do
	      (dotimes (i (cdr x))
		(setf result (* result (car x)))))
	 result))
