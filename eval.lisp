(defparameter function-probability 0.8)
(defparameter arg-probability      0.7)
(defparameter min-val               -2)
(defparameter max-val                2)
(defparameter mutate-arg           0.1)
(defparameter mutate-num           0.6)
(defparameter mutate-fun           0.2)
 
(defun mutate-min (&optional (x))
  (declare (ignore x))              -1)
(defun mutate-max (&optional (x))
  (declare (ignore x))               1)
 
(defparameter fitness-min           -7)
(defparameter fitness-max            7)
(defparameter fitness-points       100)
(defparameter fitness-step
  (coerce (/ (- fitness-max fitness-min) fitness-points) 'double-float))
(defparameter steps
  (loop for x from fitness-min to fitness-max by fitness-step collect x))
 
(defparameter population-size       50)
(defparameter individuals-survive   20)
(defparameter exception-weight    1000)
 
(defparameter gens
  #((+ expr expr)
    (- expr expr)
    (* expr expr)
    (/ expr expr)
                    ;    (expt expr expr)
    (sin expr)
    (cos expr)
    (tan expr)
    ))
 
(defparameter args
  #(x))
(defparameter args-list
  (coerce args 'list))
(defparameter gens-number (length gens))
(defparameter args-number (length args))

(defun flatten (l)
  (error "flatten")
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))
 
(defun get-args-list (fun)
  (error "get-args-list")
  (remove-duplicates
   (remove-if-not (lambda (x) (member x args-list))
          (flatten fun))))

(defun simplify-routine (arg)
  (cond
    ((or (symbolp arg) (numberp arg)) arg)
    ((listp arg)
     (let ((fun (car arg))
       (arg1 (simplify-routine (cadr arg)))
       (arg2 (simplify-routine (caddr arg))))
       (cond
     ((null arg2)
      (if (numberp arg1)
          (funcall fun arg1)
          (list fun arg1)))
     ((eq fun '+)
      (cond
        ((equal arg1 arg2)   (simplify-routine `(* 2 ,arg1)))
        ((and (numberp arg1) (numberp arg2)) (+ arg1 arg2))
        ((eq arg1 0)         arg2)
        ((eq arg2 0)         arg1)
        (t                  `(+ ,arg1 ,arg2))))
     ((eq fun '-)
      (cond
        ((equal arg1 arg2)   0)
        ((and (numberp arg1) (numberp arg2)) (- arg1 arg2))
        ((eq arg2 0)         arg1)
        (t                  `(- ,arg1 ,arg2))))
     ((eq fun '*)
      (cond
        ((and (numberp arg1) (numberp arg2)) (* arg1 arg2))
        ((eq arg1 1)         arg2)
        ((eq arg2 1)         arg1)
        (t                  `(* ,arg1 ,arg2))))
     ((eq fun '/)
      (cond
        ((or (eq arg1 0)
         (eq arg2 0))        (error "division by zero"))
        ((equal arg1 arg2)   1)
        ((eq arg2 1)         arg1)
        ((and (numberp arg1) (numberp arg2))
                             (/ arg1 arg2))
        (t                  `(/ ,arg1 ,arg2))))
     (t
      arg))))
    (t arg)))

(defun simplify (arg)
  (handler-case (simplify-routine arg)
    (error (e) (declare (ignore e)) exception-weight)))
 
(defparameter target-fun
  (eval `(lambda (x) (+ (* x 1.46327) (/ x 0.3423)))))
 
(defun random-real-interval (from to)
  (+ from (* (random 1d0) (- to from))))
 
(defun gen-fun (depth)
  (let ((prob (random 1d0)))
    (if (or (< depth 1) (> prob function-probability))
        (if (< (random 1d0) arg-probability)
            (aref args (random args-number))
            (random-real-interval min-val max-val))
        (let ((expr (aref gens (random gens-number))))
          (map 'list
               (lambda (x)
                 (if (eq x 'expr)
                     (gen-fun (1- depth))
                     x))
               expr)))))
 
(defun mutate (fun depth)
  (let ((mut (random 1d0)))
    (cond
      ((and (symbolp fun) (< mut mutate-arg))
       (gen-fun depth))
      ((and (numberp fun) (< mut mutate-num))
       (+ fun (random-real-interval (mutate-min fun) (mutate-max fun))))
      ((and (listp fun)   (< mut mutate-fun))
       (gen-fun (1- depth)))
      ((listp fun)
       (cons (car fun) (map 'list (lambda (x) (mutate x (1- depth))) (cdr fun))))
      (t
       fun))))
 
(defun test-except (depth)
  (handler-case
      (loop for x below 10 collect (eval (gen-fun depth)))
    (error (e) (print e))))
 
(defun test1 (d x)
  (let* ((ast (gen-fun d))
     (fun (eval `(lambda ,args-list ,ast)))
     (res (handler-case
          (funcall fun x)
        (error (e) e))))
    (format t "(~A)(~A) = ~A~%" ast x res)))
 
(defun compare-data (data1 data2)
  (/ (loop for d1 in data1
    for d2 in data2
    sum (abs (- (realpart d1) (realpart d2))))
     (length data1)))
 
(defun enforce-compile-1-arg (ast)
  (error "enforce-compile-1-arg")
  (let* ((sast (simplify ast))
     (sarg (get-args-list sast)))
    (if (null sarg)
    (eval `(lambda (&optional (x))
         (declare (ignore x))
         ,sast))
    (eval `(lambda ,sarg ,sast)))))
 
(defun generate-values (arg)
  (let ((fun (if (listp arg)
		 (eval `(lambda ,args-list ,(simplify arg)))
		 arg)))
    (loop for x from fitness-min to fitness-max by fitness-step
       collect (handler-case
		   (funcall fun x)
		 (error (e) (declare (ignore e)) exception-weight)))))

(defun fitness-compare (f1 f2)
  (let ((f1-data (generate-values f1))
    (f2-data (generate-values f2)))
    (/ (loop for d1 in f1-data
         for d2 in f2-data
      sum (abs (- d2 d1)))
       (length f1-data))))

(defun evaluate (d iterations fun)
  (declaim (sb-ext:muffle-conditions cl:style-warning))
  (let* ((values (generate-values fun))
	 (population
	  (coerce
	   (loop for x below population-size collect
		(let* ((fun (gen-fun d))
		       (fun-values (generate-values fun)))
		  (cons fun (compare-data values fun-values))))
	   'vector)))
    (loop for n below iterations do
	 (sort population #'< :key #'cdr)
	 (loop for i below individuals-survive do
	      (let* ((mut-fun (mutate (car (aref population i)) d))
		     (mut-values (generate-values mut-fun))
		     (result (compare-data values mut-values)))
		(when (< result (cdr (aref population i)))
		  (setf (aref population i)
			(cons mut-fun result)))))
	 (loop for i from individuals-survive below population-size do
	      (setf (aref population i)
		    (let* ((fun (gen-fun d))
			   (fun-values (generate-values fun)))
		      (cons fun (compare-data values fun-values)))))
	 (when (= 0 (rem n 100)) (format t "Iteration ~A...~%" n)))
    (declaim (sb-ext:unmuffle-conditions cl:style-warning))
    population))
