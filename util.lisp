(symbol-macrolet ((declares (when (and (consp body)
                                       (consp (car body))
                                       (equal 'declare (caar body)))
                              (setf declarations (car body)
                                    body (cdr body))))
                  (hash `(gethash (list ,@args) ,h)))
  
  (defmacro deflazy/memo (name args &body body)
    (let ((h (gensym "HASH"))
          (result (gensym "RESULT"))
          (declarations ()))
      declares
      `(let ((,h (make-hash-table :test #'equal)))
         (defun ,name ,args
           ,declarations
           (multiple-value-bind (val foundp) ,hash
             (if foundp
                 val
                 (setf ,hash
                       (lambda ()
                         (let ((,result (progn ,@body)))
                           (setf ,hash (lambda () ,result))
                           ,result)))))))))

  (defmacro defmemo (name args &body body)
    (let ((h (gensym "HASH"))
          (declarations ()))
      declares
      `(let ((,h (make-hash-table :test #'equal)))
         (defun ,name ,args
           ,declarations
           (multiple-value-bind (val foundp) ,hash
             (if foundp
                 val
                 (setf ,hash (progn ,@body)))))))))


(defmacro deflazy (name args &body body)
  `(defun ,name ,args (lambda () ,@body)))

(deflazy/memo fib-lazymemo (n)
  (declare (fixnum n))
;  (format t "Evaluating: (fib-lazymemo ~A) ...~%" n)
  (if (<= n 2)
      1
      (+ (funcall (fib-lazymemo (1- n)))
         (funcall (fib-lazymemo (- n 2))))))

(defun fib-slow (n)
  (declare (fixnum n))
;  (format t "Evaluating: (fib-slow ~A) ...~%" n)
  (if (<= n 2)
      1
      (+ (fib-slow (1- n)) (fib-slow (- n 2)))))

(defmemo fib-memo (n)
  (declare (fixnum n))
;  (format t "Evaluating: (fib-memo ~A) ...~%" n)
  (if (<= n 2)
      1
      (+ (fib-memo (1- n)) (fib-memo (- n 2)))))

(deflazy fib-lazy (n)
  (declare (fixnum n))
;  (format t "Evaluating: (fib-lazy ~A) ...~%" n)
  (if (<= n 2)
      1
      (+ (funcall (fib-lazy (1- n))) (funcall (fib-lazy (- n 2))))))
