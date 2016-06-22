;; *cmd-string* tells execSql what command to run to run a file
;; Parameters: host database filename username password
;; (defparameter *cmd-string*  "sqsh -S '~A' -D '~A' -i '~A' -U '~A' -P '~A'")

;; *interactive-cmd-string* tells execSql what command to run for an interactive shell
;; Parameters: host database username password
;; (defparameter *interactive-cmd-string* "sqsh -S '~A' -D '~A' -U '~A' -P '~A'")


;; *credentials* stores the list of servers, databases, usernames, and passwords
(defparameter *credentials*
  '(
    (server1.example.com
     (database1 "user" "password")
     (database2 "user" "password"))

    (server2.example.com
     (database1 "user" "password")
     (database2 "user" "password"))))
