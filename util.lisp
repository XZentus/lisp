(defmacro deflazy/memo (name args &body body)
  (let ((h (gensym "HASH"))
        (result (gensym "RESULT")))
    `(let ((,h (make-hash-table :test #'equal)))
       (defun ,name (,@args)
         (multiple-value-bind
               (val foundp)
             (gethash (list ,@args) ,h)
           (if foundp
               val
               (setf (gethash (list ,@args) ,h)
                     (lambda ()
                       (let ((,result (progn ,@body)))
                         (setf (gethash (list ,@args) ,h) (lambda () ,result))
                         ,result)))))))))

(defmacro defmemo (name args &body body)
  (let ((h (gensym "HASH")))
    `(let ((,h (make-hash-table :test #'equal)))
       (defun ,name (,@args)
         (multiple-value-bind
               (val foundp)
             (gethash (list ,@args) ,h)
           (if foundp
               val
               (setf (gethash (list ,@args) ,h)
                     (progn ,@body))))))))


(deflazy/memo fib-lazymemo (n)
  (if (<= n 2)
      1
      (+ (funcall (fib (1- n))) (funcall (fib (- n 2))))))

(defun fib-slow (n)
  (if (<= n 2)
      1
      (+ (fib-slow (1- n)) (fib-slow (- n 2)))))

(defmemo fib-memo (n)
  (if (<= n 2)
      1
      (+ (fib-memo (1- n)) (fib-memo (- n 2)))))
