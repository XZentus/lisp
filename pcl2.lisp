
(defun primep (n)
  (when (> n 1)
    (loop for x from 2 to (isqrt n) never (zerop (mod n x)))))

(defun next-prime (n)
  (loop for x from n when (primep x) return x))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for x in names collect `(,x (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (end-value-name)
		`(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
		      (,end-value-name ,end))
		     ((> ,var  ,end-value-name))
		   ,@body)))

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~A: ~A~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
		`(let ((,result t))
		   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
		   ,result)))

(defmacro deftest (name params &body body)
  `(defun ,name ,params
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-- ()
  (check
   (= (- 1 1) 0)
   (= (- 3 4) -1)))

(deftest test-* ()
      (check (= (+ 1 2) 3)
	   (= (+ -3 -7) -10)
	   (= (+ -3 3) 0)))

(deftest test-+ ()
      (check (= (* 20 30) 600)
	   (= (* 10 0) 0)
	   (= (* 0 10) 0)
	   (= (* -1 10) -10)
	   (= (* 3 4) (* 4 3))))

(deftest test-arithmetics ()
  (combine-results
   (test-+)
   (test--)
   (test-*)))
