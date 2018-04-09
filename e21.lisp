(defvar *sdivs* (make-array '(10001) :element-type 'fixnum :initial-element 0))
(defvar *checker* (make-array '(10001) :initial-element t))

(defun divs-sum (n)
  (loop for x from 1 upto (/ n 2)
        summing (if (= 0 (rem n x))
                    x
                    0)))

(defun fill-sdivs (&optional (lim 10000))
  (loop for i from 1 upto lim do
        (setf (aref *sdivs* i) (divs-sum i))))

(defun count-amicable (&optional (db *sdivs*))
  (setf *checker* (make-array (list (length db)) :initial-element t))
  (loop for i from 2 to (1- (length db)) summing
        (if (aref *checker* i)
             (let ((d_a (aref *sdivs* i)))
               (cond
                 ((> d_a 10000) 0)
                 ((and (/= i d_a)
                       (= i (aref *sdivs* d_a)))
                  (setf (aref *checker* i) nil
                        (aref *checker* d_a) nil)
                  (format t "~A <-> ~A~%" i d_a)
                  (+ i d_a))
                 (t 0)))
             0)))
