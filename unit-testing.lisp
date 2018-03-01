(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name params &body body)
  `(defun ,name ,params
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -4) -5)))

(deftest test-* ()
  (check
    (= (* 100500 0) 0)
    (= (* 100500 1) 100500)
    (= (* 3 7) 21)))

(deftest test-/ ()
  (check
    (= (/ 100500 1) 100500)
    (= (/ 1 10) 1/10)
    (= (/ 10 2) 5)))

(deftest test-afithmetic ()
  (combine-results
    (test-+)
    (test-*)
    (test-/)))