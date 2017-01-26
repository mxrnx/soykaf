#!/usr/bin/csi -s 

(load "conf.scm")

(use mysql-client)

(define con (make-mysql-connection mysql-host mysql-user mysql-pass mysql-schema))

(define (redirect page)
  (display (string-append "<meta http-equiv='refresh' content='0;URL=" page "' />")))

(define (make-post-list query)
  (map (lambda (s)
	 (define arg (cdr (string-split s "=")))
	 (if (not (null? arg))
	   (car arg)
	   '()))
       (string-split query "&")))

(define (add-post postlist)
  (define name (if (null? (car postlist)) "NULL" (string-append "'" (car postlist) "'")))
  (when (null? (cadr postlist)) (display "Comment empty, post discarded.") (exit 1))
  (when (not (string? (cadr postlist))) (display "Malformed request, post discarded.") (exit 1))
  (when (null? (caddr postlist)) (display "Malformed request, post discarded") (exit 1))
  (define com (cadr postlist))
  (define reply (caddr postlist))
  (display (con (string-append "insert into " table " (name, com, reply) values (" name ", '" com "', '" reply"')")))
  #t)

(if (string=? (get-environment-variable "REQUEST_METHOD") "POST")
  (begin
    (display "Content-Type: text/html\n\n")
    (add-post (make-post-list (read-line (current-input-port))))))
(redirect self)
