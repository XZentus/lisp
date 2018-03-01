
(ql:quickload :cl-ppcre)
(ql:quickload :dexador)

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

(defmethod print-object ((p person)
                         (s stream))
  (format s "~A ~A ~A ~A"
          (person-surname p)
          (person-name p)
          (person-patronymic p)
          (person-birth-date p)))

(defparameter regex-pattern
  "(\\w*)\\s+(\\w*)\\s+(\\w*)\\s+(\\d{2}\\.\\d{2}\\.\\d{4})\\s+(\\d{2})\\s*(\\d{2})\\s*(\\d{6})\\s+(\\d{2}\\.\\d{2}\\.\\d{4})")

(defun make-gibdd-url (str)
  (let ((pattern
          "(\\d{4})[\\s№]*(\\d{6})[\\sот]*([\\d\\.]{10})"))
    (multiple-value-bind (_ data)
        (ppcre:scan-to-strings pattern str)
      (declare (ignore _))
      (if data
          (concatenate 'string
                       "https://гибдд.рф/check/driver#"
                       (elt data 0)
                       (elt data 1)
                       "+"
                       (elt data 2))
          (format t "Cannot parse: ~a~%" str)))))

(defparameter *data* ())
(defparameter current-token ())
(defparameter *results* ())
(defvar days-im-months #(31 29 31 30 31 30 31 31 30 31 30 31))

(defun digit-int (d)
  (- (char-int d) (char-int #\0)))

(defun check-date (str)
  (and (= (length str) (+ 2 1 2 1 4))
       (char= (aref str 2) #\.)
       (char= (aref str 5) #\.)
       (every #'digit-char-p (loop for i in '(0 1 3 4 6 7 8 9) collect (aref str i)))
       (let* ((digits (loop for i in '(0 1 3 4 6 7 8 9) collect (digit-int (aref str i))))
              (d (+ (* (car digits) 10)
                    (cadr digits)))
              (m (+ (* (nth 2 digits) 10)
                    (nth 3 digits)))
              (y (+ (* (nth 4 digits) 1000)
                    (* (nth 5 digits) 100)
                    (* (nth 6 digits) 10)
                    (nth 7 digits))))
         (and (<= m 12)
              (<= d (aref days-im-months (- m 1)))
              (<= y 2020)
              (>= y 1920)))))

(defun js-time-approx ()
  (* 407 (get-universal-time)))

(defun read-captcha (cookies)
  (let* ((captcha-token
           (dex:get
            (concatenate 'string token-url
                         "?r=" (format nil "?r=~A" (js-time-approx)))
            :cookie-jar cookies))
         (captcha-picture
           (dex:get (concatenate 'string
                                 token-url
                                 (format nil "?r=~A" (js-time-approx))
                                 "&a=" captcha-token)
                    :cookie-jar cookies))
         (result (progn
                   (with-open-file (out tmp-file
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :element-type 'unsigned-byte)
                     (write-sequence captcha-picture out))
                   (uiop:run-program tmp-file :ignore-error-status t)
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

(defun read-command ()
  (princ "Command: ")
  (read))

(defun read-data ()
  (format t "INPUT:~%")
  (do ((line (read-line) (read-line)))
      ((string= line ""))
    (multiple-value-bind (_ parsed-data)
	(ppcre:scan-to-strings regex-pattern line)
      (declare (ignore _))
      (setf *data*
	    (if (and parsed-data
                     (check-date (aref parsed-data 3))
                     (check-date (aref parsed-data 7)))
		(nconc *data* (list (make-person :surname (aref parsed-data 0)
						 :name (aref parsed-data 1)
						 :patronymic (aref parsed-data 2)
						 :birth-date (aref parsed-data 3)
						 :passp-sn (format nil "~A ~A ~A"
								   (aref parsed-data 4)
								   (aref parsed-data 5)
								   (aref parsed-data 6))
						 :passp-date (aref parsed-data 7))))
		(nconc *data* (list line)))))))

(defun routine ()
  (setf current-token ())
  (setf *results* ())
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
                       (t (list "Wrong/incomplete data")))))))

(defun show-results ()
  (format t "RESULTS:~%")
  (loop for x in *results* do
        (format t "~A~%" x)))

(defun main ()
  (do ((command (read-command) (read-command)))
      ((or (eq command 'end)
	   (eq command 'q)
	   (eq command 'quit))
       (format t "End~%"))
    (cond
      ((or (eq command 'd) (eq command 'data))
       (format t "~A~%" *data*))
      ((or (eq command 's) (eq command 'show))
       (show-results))
      ((or (eq command 'i) (eq command 'input))
       (setf *data* ())
       (read-data))
      ((or (eq command 'a) (eq command 'append))
       (read-data))
      ((or (eq command 'c) (eq command 'check))
       (routine))
      ((or (eq command 'r) (eq command 'run))
       (setf *data* ())
       (read-data)
       (routine)
       (show-results)))))
