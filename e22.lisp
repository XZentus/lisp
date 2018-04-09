(ql:quickload :SPLIT-SEQUENCE)

(defparameter *test-string* "\"MARY\",\"PATRICIA\",\"LINDA\",\"BARBARA\",\"ELIZABETH\",\"JENNIFER\",\"MARIA\",\"SUSAN\",\"MARGARET\",\"DOROTHY\",\"LISA\",\"NANCY\",\"KAREN\",\"BETTY\",\"HELEN\",\"SANDRA\",\"DONNA\",\"CAROL\",\"RUTH\",\"SHARON\",\"MICHELLE\",\"LAURA\",\"SARAH\",\"KIMBERLY\"")

(defun str->list (&optional (str *test-string*))
  (sort (map 'vector #'read-from-string (split-sequence:split-sequence #\, str)) #'string<))

(defun str->score (str)
  (loop for x across str summing (1+ (- (char-code x)
                                        (char-code #\A)))))

(defun names-vec->scores (db)
  (loop for x from 1
        for name across db summing
        (* x (str->score name))))

(defun main ()
  (with-open-file (in (merge-pathnames "../src/lisp/p022_names.txt"))
    (names-vec->scores (str->list (read-line in)))))
