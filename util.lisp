(defmacro deflazy/memo (name args &body body)
  (let ((h (gensym "HASH"))
        (result (gensym "RESULT"))
        (declarations ()))
    (when (and (listp body)
               (listp (car body))
               (equal 'declare (caar body)))
      (setf declarations (car body)
            body (cdr body)))
    `(let ((,h (make-hash-table :test #'equal)))
       (defun ,name (,@args)
         ,declarations
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
  (let ((h (gensym "HASH"))
        (declarations ()))
    (when (and (listp body)
               (listp (car body))
               (equal 'declare (caar body)))
      (setf declarations (car body)
            body (cdr body)))
    `(let ((,h (make-hash-table :test #'equal)))
       (defun ,name (,@args)
         ,declarations
         (multiple-value-bind
               (val foundp)
             (gethash (list ,@args) ,h)
           (if foundp
               val
               (setf (gethash (list ,@args) ,h)
                     (progn ,@body))))))))

(deflazy/memo fib-lazymemo (n)
  (declare (fixnum n))
;  (format t "Evaluating: (fib-lazymemo ~A) ...~%" n)
  (if (<= n 2)
      1
      (+ (funcall (fib-lazymemo (1- n))) (funcall (fib-lazymemo (- n 2))))))

(defun fib-slow (n)
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
