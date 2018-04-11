(defparameter *input* "L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4")

(defstruct pos x y)

(defparameter *p* (make-pos :x 0 :y 0))

(defun make-step (dir val)
  (case (rem dir 4)
    (0 (incf (pos-y *p*) val)) ; ^
    (1 (incf (pos-x *p*) val)) ; >
    (2 (decf (pos-y *p*) val)) ; v
    (3 (decf (pos-x *p*) val)) ; <
    ))

;; 0
;;3 1
;; 2
(defun change-dir (dir rot)
  (case rot
    (#\R (1+ dir))
    (#\L (if (< dir 1) 3 (1- dir)))))

(defun dir-symb (n)
  (case (rem n 4)
    (0 #\^) ; ^
    (1 #\>) ; >
    (2 #\v) ; v
    (3 #\<) ; <
    ))

(defun main ()
  (setf *p* (make-pos :x 0 :y 0))
  (let ((accum 0)
        (dir 0))
    (loop for c across *input*
          finally (make-step dir accum) do
          (cond
            ((or (eq c #\R)
                 (eq c #\L))
             (setf dir (change-dir dir c)))
            ((digit-char-p c)
             (setf accum (+ (* accum 10)
                            (- (char-code c) (char-code #\0)))))
            ((eq c #\,)
             (make-step dir accum)
             (setf accum 0))
            (t)))
    (format t "POS: ~A~%DIST: ~A~%" *p* (+ (abs (pos-x *p*)) (abs (pos-y *p*))))))

(defun main2 ()
  (let ((data (make-hash-table :key #'equal))
        (accum 0)
        (dir 0))
    (loop for c across *input*
          finally (make-step dir accum) do
          (cond
            ((or (eq c #\R)
                 (eq c #\L))
             (setf dir (change-dir dir c)))
            ((digit-char-p c)
             (setf accum (+ (* accum 10)
                            (- (char-code c) (char-code #\0)))))
            ((eq c #\,)
             (make-step dir accum)
             (setf accum 0))
            (t)))
    (format t "POS: ~A~%DIST: ~A~%" *p* (+ (abs (pos-x *p*)) (abs (pos-y *p*))))))
