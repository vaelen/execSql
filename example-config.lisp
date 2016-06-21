;; *cmd-string* tells execSql what command to run
;; Parameters: host database filename username password
;; (defparameter *cmd-string*  "sqsh -S '~A' -D '~A' -i '~A' -U '~A' -P '~A'")

;; *credentials* stores the list of servers, databases, usernames, and passwords
(defparameter *credentials*
  '(
    (server1.example.com
     (database1 "user" "password")
     (database2 "user" "password"))

    (server2.example.com
     (database1 "user" "password")
     (database2 "user" "password"))))
