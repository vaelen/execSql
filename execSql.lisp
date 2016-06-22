(require 'uiop)

(defvar *execsql-version* '(1 1))

(defvar *execsql-version-string*
  (format 'nil "~A.~A"
          (car *execsql-version*)
          (cadr *execsql-version*)))

(defparameter *credentials* 'nil)

;; Parameters: host database filename username password
(defparameter *cmd-string* "sqsh -S '~A' -D '~A' -i '~A' -U '~A' -P '~A'")

;; Parameters: host database username password
(defparameter *interactive-cmd-string* "sqsh -S '~A' -D '~A' -U '~A' -P '~A'")

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

(defun execute-sql (host database &optional filename username password)
  (if (and username password)
      (if filename
          (let ((cmd (format 'nil *cmd-string* host database filename username password)))
            (format t "Executing SQL. Server: ~A, Database: ~A, File: ~A~C"
                    host database filename #\newline)
            ;;(princ cmd)
            ;;(princ #\newline)
            (uiop:run-program cmd :input :interactive :output :interactive :error :interactive))
          (let ((cmd (format 'nil *interactive-cmd-string* host database username password)))
            (format t "Executing SQL Shell. Server: ~A, Database: ~A~C"
                    host database #\newline)
            ;;(princ cmd)
            ;;(princ #\newline)
            (uiop:run-program  cmd :input :interactive :output :interactive :error :interactive)))
      (if (valid-host host)
          (if (valid-database host database)
              (if (or (not filename) (probe-file filename))
                  (let* ((cred (cdr (valid-database host database)))
                         (username (car cred))
                         (password (cadr cred)))
                    (cond ((and username password)
                           (execute-sql host database filename username password))
                          ((not username)
                           (format t "No Username Found for Host: ~S, Database: ~S~C"
                                   (to-symbol host) (to-symbol database) #\newline))
                          ((not password)
                           (format t "No Password Found for User: ~S, Host: ~S, Database: ~S~C"
                                   username (to-symbol host) (to-symbol database) #\newline))))
                  (format t "File Not Found: ~A~C"
                          filename #\newline))
              (format t "Invalid Database: ~A. Valid Databases: ~A~C"
                      (to-symbol database) (databases host) #\newline))
          (format t "Invalid Host: ~A. Valid Hosts: ~A~C"
                  (to-symbol host) (hosts) #\newline))))

(defun print-help-hosts ()
  (format t
          "~CHosts: ~A~C"
          #\tab (hosts) #\newline))

(defun print-help-databases (host)
  (format t
          "~CDatabases on ~A: ~A~C"
          #\tab (to-symbol host) (databases host) #\newline))

(defun print-help (&optional host database)
  (format t
          "Usage: execSql <server> <database> [-f filename] [-u username] [-p password]~C"
          #\newline)
  (cond ((not host) (print-help-hosts))
        ((not database) (print-help-databases host))))

(defun print-version ()
  (format t "execSql version ~A~C" *execsql-version-string* #\newline))

(defun remove-options-with-arguments (argv options)
  (if options
      (let ((option (car options))
            (rest (cdr options)))
        (if (member option argv :test #'equal)
            (let ((p (position option argv :test #'equal)))
              (remove-options-with-arguments
               (append (subseq argv 0 p) (subseq argv (+ p 2)))
               rest))
            (remove-options-with-arguments argv rest)))
      argv))

(defun remove-options (argv)
  (reverse
   (set-difference
    (remove-options-with-arguments argv '("-f" "-u" "-p"))
    '("-h" "-H" "--help" "-v" "--version") :test #'equal)))

(defun main (argv)
  (if (probe-file *configFile*)
      (load *configFile*))
  (let (
        (filename (cadr (member "-f" argv :test #'equal)))
        (username (cadr (member "-u" argv :test #'equal)))
        (password (cadr (member "-p" argv :test #'equal))))
    (cond ((or (member "-h" argv :test #'equal)
               (member "-H" argv :test #'equal)
               (member "--help" argv :test #'equal))
           (print-help))
          ((or (member "-v" argv :test #'equal)
               (member "--version" argv :test #'equal))
           (print-version))
          (t
           (let* ((args (remove-options (cdr argv)))
                  (host (car args))
                  (database (cadr args)))
             (if (and host database)
                 (execute-sql host database filename username password)
                 (print-help host database))
             )))))
            
            

