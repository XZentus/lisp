(ql:quickload :cl-ppcre)
(ql:quickload :dexador)
(ql:quickload :com.gigamonkeys.utilities)

(defvar token-url "https://service.nalog.ru/static/captcha.html")
(defvar tmp-file "tmp.gif")
(defvar *main-url* "https://service.nalog.ru/inn.do")
(defvar *request-url* "https://service.nalog.ru/inn-proc.do")
(defvar *cookie-jar* (cl-cookie:make-cookie-jar))

(defstruct person
  surname
  name
  patronymic
  birth-date
  passp-sn
  passp-date)

(defparameter regex-pattern
  "(\\w*)\\t(\\w*)\\t(\\w*)\\t(\\d{2}\\.\\d{2}\\.\\d{4})\\t(\\d{2}\\s\\d{2}\\s\\d{6})\\t(\\d{2}\\.\\d{2}\\.\\d{4})")

(defparameter *data* ())
(defparameter current-token ())
(defparameter *results* ())

(defun read-captcha (cookies)
  (let* ((captcha-token
	  (dex:get
	   (concatenate 'string token-url
			"?r=" (format nil "?r=~A"
				      (com.gigamonkeys.utilities::javascript-time)))
	   :cookie-jar cookies))
	 (captcha-picture
	  (dex:get (concatenate 'string
				token-url
				(format nil "?r=~A"
					(com.gigamonkeys.utilities::javascript-time))
				"&a=" captcha-token)
		   :cookie-jar cookies))
	 (result (progn
		   (with-open-file (out tmp-file
					:direction :output
					:if-exists :supersede
					:if-does-not-exist :create
					:element-type 'unsigned-byte)
		     (write-sequence captcha-picture out))
		   (asdf:run-shell-command tmp-file)
		   (read-line))))
    (if (string= result "")
	(read-captcha cookies)
	(cons result captcha-token))))

(defun make-request (p cookies)
  (do ((inn 'error))
      ((not (eql inn 'error)) (list inn))
    (handler-case
	(let* ((captcha-result
		(progn
		  (format t "Person: ~A~%Captcha: " p)
		  (read-captcha cookies)))
	       (ans (dex:post *request-url*
			      :cookie-jar cookies
			      :content `(("c" . "innMy")
					 ("fam" . ,(person-surname p))
					 ("nam" . ,(person-name p))
					 ("otch" . ,(person-patronymic p))
					 ("bdate" . ,(person-birth-date p))
					 ("bplace" . "")
					 ("doctype" . "21")
					 ("docno" . ,(person-passp-sn p))
					 ("docdt" . ,(person-passp-date p))
					 ("captcha" . ,(car captcha-result))
					 ("captchaToken" . ,(cdr captcha-result))))))
	  (setf inn (ppcre:scan-to-strings "\\d{12}" ans)))
      (error (x)
	(format t "Error: ~A~%" x)
	(setf inn 'error)))))

(defun main ()
  (setf *data* ())
  (setf current-token ())
  (setf *results* ())
  (format t "INPUT:~%")

  (do ((line (read-line) (read-line)))
      ((string= line ""))
    (multiple-value-bind (_ parsed-data)
	(ppcre:scan-to-strings regex-pattern line)
      (declare (ignore _))
      (setf *data*
	    (if parsed-data
		(nconc *data* (list (make-person :surname (aref parsed-data 0)
						 :name (aref parsed-data 1)
						 :patronymic (aref parsed-data 2)
						 :birth-date (aref parsed-data 3)
						 :passp-sn (aref parsed-data 4)
						 :passp-date (aref parsed-data 5))))
		(nconc *data* (list line))))))
  (format t "WORKING...~%")

  (multiple-value-bind (_ token)
      (ppcre:scan-to-strings "name=\"captchaToken\" value=\"([0-9A-Z]+)\"" 
			     (dex:get *main-url* :cookie-jar *cookie-jar*))
    (declare (ignore _))
    (setf current-token (aref token 0)))

  (loop for p in *data* do
       (setf *results*
	     (nconc *results*
		    (cond
		      ((person-p p) (make-request p *cookie-jar*))
		      (t (list "Недостаточно данных для запроса"))))))
  (format t "RESULTS:~%")

  (loop for x in *results* do
       (format t "~A~%" x)))
