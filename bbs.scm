#!/usr/bin/csi -s 

;;; CONFIG STARTS HERE ;;;
(define title "scheme bbs")		; the title of your board
(define subtitle "tiny scheme board")	; tagline displayed under the title

(define defname "anonymous")		; name to use when user doesn't fill one in

(define self "bbs.scm")			; name of this file

(define mysql-host "localhost")
(define mysql-user "root")
(define mysql-pass "welkom123")
(define mysql-schema "shima")
;;; CONFIG ENDS HERE ;;;

(use mysql-client)
(define con (make-mysql-connection mysql-host mysql-user mysql-pass mysql-schema))

(define (table-exists? table)
  (define fetch (con (string-append "show tables like '" table "'")))
  (define first (fetch))
  (if (or (mysql-null? first) (eqv? #f first))
    #f
    #t))

(define (table-create table)
  (con (string-append "create table " table " (primary key(no), no int not null auto_increment, name text, com text not null)")))

(define (tag t cont)
  (string-append "<" t ">" cont "</" t ">"))

(define (div id cont)
  (string-append "<div id='" id "'>" cont "</div> "))

(define (divc id cont)
  (string-append "<div class='" id "'>" cont "</div> "))

(define (span class cont)
  (string-append "<span class='" class "'>" cont "</span> "))

(define encodingtag "<meta content='utf-8' />")

(define (fancytitle) (string-append "&lambda;::<a href='" self "'>" title "</a>"))

(define (wrap header body)
  (string-append "<!doctype html>" (tag "html" (string-append (tag "head" header) (tag "body" body)))))

(define (formatpost row)
  (divc "post" (string-append
		 (span "num" (car row))
		 (span "name" (if (mysql-null? (cadr row))
				defname
				(cadr row)))
		 (span "com" (caddr row)))))

(define (display-post curs fetch)
  (define row (fetch))
  (if (not (or (mysql-null? row) (eqv? #f row)))
    (begin
      (define curs (string-append curs (formatpost row)))
      (display-post curs fetch))
    curs))

(define (display-posts)
  (define fetch (con "select * from posts"))
  (display-post "" fetch))

(define (displayheader)
  (display "Content-Type: text/html")
  (newline)
  (newline))

(define (displaypage)
  (display
    (wrap 
      (string-append (tag "title" title) encodingtag)
      (string-append (tag "h1" (fancytitle)) (tag "h2" subtitle) (div "posts" (display-posts))))))

(define displayfooter 0)

; prepare
(if (not (table-exists? "posts"))
  (table-create "posts"))

; show page
(displayheader)
(displaypage)
(newline)
