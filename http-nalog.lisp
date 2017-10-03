(defparameter token-url "https://service.nalog.ru/static/captcha.html")
(defparameter tmp-file "d:/src/lisp/tmp.gif")
(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))

(dex:head "https://service.nalog.ru/inn.do" :cookie-jar *cookie-jar*)

(defparameter current-token (dex:get token-url :cookie-jar *cookie-jar*))
(defparameter captcha-picture (dex:get
			       (concatenate 'string token-url "?a=" current-token)
			       :cookie-jar *cookie-jar*))

(with-open-file (out tmp-file
		     :direction :output
		     :if-exists :supersede
		     :element-type 'unsigned-byte)
	   (write-sequence captcha-picture out))

(asdf:run-shell-command tmp-file)
(defparameter captcha-result (read-line))

(defparameter ans (dex:post "https://service.nalog.ru/inn-proc.do"
			    :cookie-jar *cookie-jar*
			    :content '(("c" . "innMy")
				       ("fam" . "test")
				       ("nam" . "test")
				       ("otch" . "test")
				       ("bdate" . "11.11.1111")
				       ("bplace" . "")
				       ("doctype" . "21")
				       ("docno" . "11 11 111111")
				       ("docdt" . "11.11.1111")
				       ("captcha" . captcha-result)
				       ("captchaToken" . current-token))))

