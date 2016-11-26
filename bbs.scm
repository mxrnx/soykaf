#!/usr/bin/csi -s 

(load "conf.scm")
(load "tag.scm")

(use regex)
(use mysql-client)

; sql 
(define con (make-mysql-connection mysql-host mysql-user mysql-pass mysql-schema))

(define (table-exists? table)
  (define fetch (con (string-append "show tables like '" table "'")))
  (define first (fetch))
  (if (or (mysql-null? first) (eqv? #f first))
    #f
    #t))

(define (table-create table)
  (con (string-append "create table " table " (primary key(no), no int not null auto_increment, name text, com text not null)")))

(define (make-post-list query)
  (map (lambda (s)
	 (define arg (cdr (string-split s "=")))
	 (if (not (null? arg))
	   (car arg)
	   '()))
       (string-split query "&")))

(define (add-post postlist)
  (define name (if (null? (car postlist)) defname (car postlist)))
  (when (null? (cadr postlist)) (display "Comment empty, post discarded.") (exit 1))
  (when (not (string? (cadr postlist))) (display "Malformed request, post discarded.") (exit 1))
  (define com (cadr postlist))
  (display (con (string-append "insert into " table " (name, com) values ('" name "', '" com "')")))
  #t)

; html bits
(define (url-decode html-string)			; TODO: automatically do this
  (string-translate* html-string '(("%0D%0A" . "<br />")
				   ("%0D" . "<br />")
				   ("%21" . "!")
				   ("%26" . "&")
				   ("%2B" . "+")
				   ("%2C" . ",")
				   ("%2E" . ",")
				   ("%3B" . ";")
				   ("%3F" . "?")
				   ("+" . " ")
				   ("%23955" . "&lambda;")
				   ("%25" . "%"))))


(define (fancytitle) (string-append "&lambda;::<a href='" self "'>" title "</a>"))

(define (wrap header body)
  (string-append "<!doctype html>" (tag-s "html" (string-append (tag-s "head" header) (tag-s "body" body)))))

(define (display-postform)
  (string-append "<form action='" self "' method='post'><input type='text' name='name' /><input type='submit' value='Post' /><br /> <textarea name='com'></textarea></form"))

(define (format-post row)
  (div-c "post" (string-append
		  (tag "span" #f "num" (car row))
		  (tag "span" #f "name" (if (mysql-null? (cadr row))
					  defname
					  (cadr row)))
		  (tag "span" #f "com" (url-decode (caddr row))))))

(define (display-post curs fetch)
  (define row (fetch))
  (if (not (or (mysql-null? row) (eqv? #f row)))
    (begin
      (define curs (string-append curs (format-post row)))
      (display-post curs fetch))
    curs))

(define (display-posts)
  (define fetch (con "select * from posts"))
  (display-post "" fetch))

(define display-foot " - soykaf bbs - ")

(define (displayheader)
  (display "Content-Type: text/html")
  (newline)
  (newline))

(define (displaypage)
  (display
    (wrap 
      (string-append (tag-s "title" title) encodingtag)
      (string-append (tag-s "h1" (fancytitle)) (tag-s "h2" subtitle) (div "postform" (display-postform)) (div "posts" (display-posts)) (div "foot" display-foot)))))

(define (refresh) (display (string-append "<meta http-equiv='refresh' content='1;URL=" self "' />")))

; prepare
(if (not (table-exists? "posts"))
  (table-create "posts"))

; show page
;(if (eqv? (get-environment-variable "REQUEST_METHOD") #f) (exit 1)) ; not a cgi environment
(if (string=? (get-environment-variable "REQUEST_METHOD") "POST")
  (begin
    (displayheader)
    (if (add-post (make-post-list (read-line (current-input-port))))
      (display "Post added. Refreshing...")
      (display "Could not post. Did you fill in a comment?"))
    (refresh))
  (begin
    (displayheader)
    (displaypage)))
(newline)
