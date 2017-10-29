(defparameter *min* 100)
(defparameter *max* 999)
(defparameter *result* 0)

(defun is-palindrome (str)
  (let* ((l (length str)))
    (do ((i 0 (1+ i))
	 (j (1- l) (1- j)))
	((>= i j) t)
      (when (char/= (aref str i) (aref str j))
	(return nil)))))
    

(loop for x from *min* upto *max* do
     (loop for y from (1+ x) upto *max* do
	  (let* ((prod (* x y))
		 (str (format nil "~A" prod)))
	    (when (and (> prod *result*)
		       (is-palindrome str))
	      (setf *result* prod)))))

(print *result*)
