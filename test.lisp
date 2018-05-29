(defun fact-cps (n &optional (k #'values))
  (if (< n 2)
      (funcall k 1)
      (fact-cps (- n 1)
                (lambda (v)
                  (funcall k (* v n))))))


(defun scope-test ()
  (declare (special y))
  y)

(defun optional-test (&optional (x 1 was-set))
  (list x was-set))

(defun setf-test (vec val)
  (setf (aref vec 2) val))

(defun calc-step (arg)
  (let ((new (1- arg)))
    (format t " ~A -> ~A~%" arg new)
    new))

(defvar cont ())

(defun rec-test (arg &optional (e 100500))
  (labels ((fun1 (arg)
             (cond
               ((<= arg 0) e)
               ((= arg 5)
                (setf cont (lambda (a) (setf e a) (fun2 a)))
                (fun2 (calc-step arg)))
               (t
                (princ 1) (fun2 (calc-step arg)))))
           (fun2 (arg)
             (cond
               ((<= arg 0) 'fun2-end)
               (t
                (princ 2) (fun1 (calc-step arg))))))
    (fun1 arg)))
