(require 'uiop)

(defparameter *credentials* 'nil)

(defparameter *cmd-string*
  ; host database filename username password
  "sqsh -S '~A' -D '~A' -i '~A' -U '~A' -P '~A'")

(defparameter *configFile* "~/.execSql/config.lisp")

(defun to-symbol (x)
  (if (symbolp x)
      x
      (intern (string-upcase x))))

(defun valid-host (host)
  (assoc (to-symbol host) *credentials*))

(defun valid-database (host database)
  (assoc (to-symbol database)
         (cdr (valid-host host))))

(defun hosts ()
  (mapcar 
   (lambda (x) (car x))
   *credentials*))

(defun databases (host)
  (mapcar (lambda (x) (car x)) (cdr (valid-host host))))

(defun execute-sql (host database filename &optional username password)
  (print "execute-sql")
  (if (and username password)
      (let ((cmd (format 'nil *cmd-string* host database filename username password)))
        (format t "Executing SQL. Server: ~A, Database: ~A, File: ~A ~C"
                host database filename #\newline)
        (princ cmd)
        (princ #\newline)
        (uiop:run-program cmd :output t))
      (if (valid-host host)
          (if (valid-database host database)
              (if (probe-file filename)
                  (let* ((cred (cdr (valid-database host database)))
                         (username (car cred))
                         (password (cadr cred)))
                    (cond ((and username password)
                           (execute-sql host database filename username password))
                          ((not username)
                           (format t "No Username Found for Host: ~S, Database: ~S ~C"
                                   (to-symbol host) (to-symbol database) #\newline))
                          ((not password)
                           (format t "No Password Found for User: ~S, Host: ~S, Database: ~S ~C"
                                   username (to-symbol host) (to-symbol database) #\newline))))
                  (format t "File Not Found: ~A~C"
                          filename #\newline))
              (format t "Invalid Database: ~A. Valid Databases: ~A~C"
                      (to-symbol database) (databases host) #\newline))
          (format t "Invalid Host: ~A. Valid Hosts: ~A~C"
                  (to-symbol host) (hosts) #\newline))))

(defun print-help-hosts ()
  (format t
          "~C Hosts: ~A ~C"
          #\tab (hosts) #\newline))

(defun print-help-databases (host)
  (format t
          "~C Databases on ~A: ~A ~C"
          #\tab (to-symbol host) (databases host) #\newline))

(defun print-help (&optional host database)
  (format t
          "Usage: execSql <server> <database> <filename> [username password] ~C"
          #\newline)
  (cond ((not host) (print-help-hosts))
        ((not database) (print-help-databases host))))

(defun main (argv)
  (if (probe-file *configFile*)
      (load *configFile*))
  (if (> (length argv) 3)
      (apply #'execute-sql (cdr argv))
      (print-help (cadr argv) (caddr argv))))

