(defmacro deflazy/memo (name args &body body)
  (let ((h (gensym "hash"))
        (result (gensym "result")))
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
