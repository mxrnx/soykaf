#!/usr/bin/csi -s 

(load "conf.scm")

(use mysql-client)

(define con (make-mysql-connection mysql-host mysql-user mysql-pass mysql-schema))

(define (valid? passwd) (string=? passwd (string-append "managepass=" adminpass)))
(define passwd (get-environment-variable "HTTP_COOKIE"))

(define (rm-post! inp)
  (define no (cadr (string-split inp "=")))
  (con (string-append "delete from " table " where no='" no "'"))
  (display (string-append "Deleted post no. " no)))

(display "Content-Type: text/html\n\n")
(if (and (string=? (get-environment-variable "REQUEST_METHOD") "POST") (not (or (eq? passwd #f) (not (valid? passwd)))))
  (begin
    (rm-post! (read-line (current-input-port)))))
(display (string-append "<meta http-equiv='refresh' content='2;URL=" self "' />"))
