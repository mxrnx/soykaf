#!/usr/bin/csi -s 

(load "conf.scm")

(use mysql-client)

(define con (make-mysql-connection mysql-host mysql-user mysql-pass mysql-schema))


(if (string=? (get-environment-variable "REQUEST_METHOD") "POST")
  (begin
    (display "Content-Type: text/html\n\n")
    (display (string-append "<meta http-equiv='set-cookie' content='" (read-line (current-input-port)) "; expires=Wed, Jan 01 2020 2:02:02 GMT' />"))))
(display (string-append "<meta http-equiv='refresh' content='0;URL=" self "?manage' />"))
