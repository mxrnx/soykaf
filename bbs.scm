#!/usr/bin/csi -s 

(load "conf.scm")
(load "tag.scm")

(use mysql-client)
(use regex)
(require-extension irregex)

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

; html bits
(define (url-decode html-string)			; HACK
  (string-translate* html-string '(("%0D%0A" . "<br />")
				   ("%0D" . "<br />")
				   ("%21" . "!")
				   ("%23" . "#")
				   ("%24" . "$")
				   ("%26" . "&")
				   ("%2B" . "+")
				   ("%2C" . ",")
				   ("%2E" . ",")
				   ("%3B" . ";")
				   ("%3C" . "&lt;")
				   ("%3E" . "&gt;")
				   ("%3F" . "?")
				   ("%40" . "@")
				   ("%5E" . "^")
				   ("+" . " ")
				   ("%23955" . "&lambda;")
				   ("%25" . "%"))))

(define (apply-markup text)
  (string-substitute "((?!&gt;)|^)&gt;(.*?)<br \/>" "\\1<span class='quote'>&gt;\\2</span><br />" (string-substitute "&gt;&gt;([0-9]+?)" "<a href='#r\\1'>&gt;&gt;\\1</a>" text)))

(define (fancytitle) (string-append "&lambda;::<a href='" self "'>" title "</a>"))

(define (make-logo)
  (div "logo" (string-append (tag-s "h1" (fancytitle)) (tag-s "h2" subtitle) (a self "update") " / " (a "manage.scm" "manage") " / " (a "" "nothing"))))

(define (wrap header body)
  (string-append "<!doctype html>" (tag-s "html" (string-append (tag-s "head" header) (tag-s "body" body)))))

(define (make-postform)
  (string-append "<form action='submit.scm' method='post'><label for='name'>name</label> <input type='text' name='name' /><input type='submit' value='Post' /><br /> <textarea name='com'></textarea></form>"))

(define (format-post row)
  (div-c "post" (string-append
		  "<a href='" self "#r" (car row) "'>" (tag "span" (string-append "r" (car row)) "num" (car row)) "</a>"
		  " "
		  (tag "span" #f "name" (if (mysql-null? (cadr row))
					  defname
					  (url-decode (cadr row))))
		  " "
		  (tag "span" #f "com" (apply-markup (url-decode (caddr row)))))))

(define (make-post curs fetch)
  (define row (fetch))
  (if (not (or (mysql-null? row) (eqv? #f row)))
    (begin
      (define curs (string-append curs (format-post row)))
      (make-post curs fetch))
    curs))

(define (make-posts)
  (define fetch (con "select * from posts order by no asc"))
  (make-post "" fetch))

(define make-foot "- <a href='https://github.com/knarka/soykaf/'>soykaf</a> -")

(define (display-header)
  (display "Content-Type: text/html")
  (newline)
  (newline))

(define (display-page)
  (display
    (wrap 
      (string-append (tag-s "title" title) "<link rel='stylesheet' href='style.css' />")
      (string-append (make-logo) (div "postform" (make-postform)) (div "posts" (make-posts)) (div "foot" make-foot)))))

(define (refresh) (display (string-append "<meta http-equiv='refresh' content='1;URL=" self "' />")))

; prepare
(if (not (table-exists? "posts"))
  (table-create "posts"))

; show page
(if (eqv? (get-environment-variable "REQUEST_METHOD") #f) (exit 1)) ; not a cgi environment
(display-header)
(display-page)
(newline)
