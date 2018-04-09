(defparameter *lim* (expt 10 5))

(defparameter *sdivs* (make-array (list (1+ *lim*)) :element-type 'fixnum :initial-element 0))
(defparameter *checker* (make-array (list (1+ *lim*)) :initial-element t))

(defun divs-sum (n)
  (loop for x from 1 upto (/ n 2)
        summing (if (= 0 (rem n x))
                    x
                    0)))

(defun fill-sdivs (&optional (lim *lim*))
  (loop for i from 1 upto lim do
        (setf (aref *sdivs* i) (divs-sum i))))

(defun count-amicable (&optional (lim *lim*))
  (when (= lim 1)
    (return-from count-amicable 0))
  (setf *checker* (make-array (list (1+ *lim*)) :initial-element t))
  (loop for i from 2 to lim summing
        (if (aref *checker* i)
            (let ((d_a (aref *sdivs* i)))
              (cond
                ((or (> d_a *lim*) (< d_a i)) 0)
                ((and (/= i d_a)
                      (= i (aref *sdivs* d_a)))
                 (setf (aref *checker* i) nil
                       (aref *checker* d_a) nil)
                                        ;                  (format t "~A <-> ~A~%" i d_a)
                 (+ i d_a))
                (t 0)))
            0)))

(defun main ()
  (fill-sdivs)
  (dotimes (i (read))
    (format t "~A~%" (count-amicable (read)))))
