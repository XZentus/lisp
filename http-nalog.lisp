(ql:quickload :cl-ppcre)
(ql:quickload :dexador)
(ql:quickload :com.gigamonkeys.utilities)

(defvar token-url "https://service.nalog.ru/static/captcha.html")
(defvar tmp-file "d:/src/lisp/tmp.gif")
(defvar *main-url* "https://service.nalog.ru/inn.do")
(defvar *request-url* "https://service.nalog.ru/inn-proc.do")
(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))

(defstruct person
  surname
  name
  patronymic
  birth-date
  passp-sn
  passp-date)

(defvar regex-pattern
  "(\\w*)\\t(\\w*)\\t(\\w*)\\t(\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d)\\t(\\d{2,2}\\s\\d{2,2}\\s\\d{6,6})\\t(\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d)")

(defparameter *data* ())

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

(defparameter current-token ())

(multiple-value-bind (_ token)
    (ppcre:scan-to-strings "name=\"captchaToken\" value=\"([0-9A-Z]+)\"" 
			   (dex:get *main-url* :cookie-jar *cookie-jar*))
  (declare (ignore _))
  (setf current-token (aref token 0)))

(defparameter *results* ())

(defun read-captcha (cookies)
  (let* ((captcha-token (dex:get
			 (concatenate 'string token-url
				      "?r=" (format nil "?r=~A"
						    (com.gigamonkeys.utilities::javascript-time)))
			 :cookie-jar cookies))
	 (captcha-picture (dex:get (concatenate 'string
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

(loop for p in *data* do
     (setf *results*
	   (nconc *results*
		  (cond
		    ((person-p p)
		     (let* ((captcha-result (read-captcha *cookie-jar*))
			    (ans (dex:post *request-url*
					   :cookie-jar *cookie-jar*
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
		       (list (ppcre:scan-to-strings "\\d{12,12}" ans))))
		    (t (list "Недостаточно данных для запроса"))))))

(loop for x in *results* do
     (format t "~A~%" x))

