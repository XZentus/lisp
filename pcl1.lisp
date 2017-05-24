(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (format t "~{~{~A~10T~A~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))
     
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
	(print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun delete-rows (selector)
  (setf *db* (remove-if selector *db*)))

(defun make-comparsion-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparsion-list (fields)
  (loop while fields
       collecting (make-comparsion-expr (pop fields) (pop fields))))

(defun select (selector)
  (remove-if-not selector *db*))

(defmacro where (&rest clauses)
  `(lambda (cd) (and ,@(make-comparsion-list clauses))))

(defun update (selector &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar (lambda (row)
		  (when (funcall selector row)
		    (if title    (setf (getf row :title)  title))
		    (if artist   (setf (getf row :artist) title))
		    (if rating   (setf (getf row :rating) rating))
		    (if ripped-p (setf (getf row :ripped) ripped)))
		  row) *db*)))
