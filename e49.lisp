(defparameter *limit* 10001)

(defparameter *erast-primes*
  (let ((result (make-array (list (1+ *limit*)) :element-type 'bit :initial-element 1)))
    (do ((i 2 (1+ i)))
        ((>= (* i i) *limit*))
      (do ((composite (* i i) (+ composite i)))
          ((>= composite *limit*))
        (setf (aref result composite) 0)))
    result))

(defun make-pattern (a)
  (let ((pat (make-array '(10) :element-type '(integer 0 255) :initial-element 0)))
    (tagbody
     start
       (multiple-value-bind (d m) (floor a 10)
         (incf (aref pat m))
         (setf a d))
       (when (> a 0)
         (go start)))
    pat))

(defun is-permut-digits (a b c)
  (let ((a-pat (make-pattern a)))
    (and (equalp a-pat (make-pattern b))
         (equalp a-pat (make-pattern c)))))

(defun next-prime (start)
  (cond
    ((>= start *limit*)
     (+ *limit* 100500))
    ((evenp start) (next-prime (1+ start)))
    (t
     (do ((i start (+ i 2)))
         ((or (>= i *limit*)
              (/= (aref *erast-primes* i) 0))
          i)))))

(defun check-seq (a b)
  (let ((c (+ b (- b a))))
    (and (<= a *limit*)
         (<= b *limit*)
         (<= c *limit*)
         (= 1 (aref *erast-primes* c))
         (is-permut-digits a b c)
         (list a b c))))

(defun routine ()
  (let ((result ()))
    (do ((a 1000 (next-prime (1+ a))))
        ((>= a 9999))
      (do ((b (next-prime (1+ a)) (next-prime (1+ b))))
          ((>= (+ b b (- a)) *limit*))
        (let ((check (check-seq a b)))
          (when check
              (push check result)))))
    result))
