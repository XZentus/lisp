(ql:quickload :cl-ppcre)
(ql:quickload :dexador)

(defparameter token-url "https://service.nalog.ru/static/captcha.html")
(defparameter tmp-file "d:/src/lisp/tmp.gif")
(defvar *cookie-jar* (cl-cookie:make-cookie-jar))

(defvar *main-url* "https://service.nalog.ru/inn.do")

(defvar current-token)

(multiple-value-bind (_ token)
    (ppcre:scan-to-strings "name=\"captchaToken\" value=\"([0-9A-Z]+)\"" 
			   (dex:get *main-url* :cookie-jar *cookie-jar*))
  (setf current-token (aref token 0)))

(defparameter captcha-picture (dex:get
			       (concatenate 'string token-url
					    "?a=" current-token)
			       :cookie-jar *cookie-jar*))

(with-open-file (out tmp-file
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create
		     :element-type 'unsigned-byte)
  (write-sequence captcha-picture out))

(asdf:run-shell-command tmp-file)
(defparameter captcha-result (read-line))

(defparameter ans (dex:post "https://service.nalog.ru/inn-proc.do"
			    :cookie-jar *cookie-jar*
			    :content '(("c" . "innMy")
				       ("fam" . "тест")
				       ("nam" . "тест")
				       ("otch" . "тест")
				       ("bdate" . "11.11.1111")
				       ("bplace" . "")
				       ("doctype" . "21")
				       ("docno" . "11 11 111111")
				       ("docdt" . "11.11.1111")
				       ("captcha" . captcha-result)
				       ("captchaToken" . current-token))))

