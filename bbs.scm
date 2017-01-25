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
  (con (string-append "create table " table " (primary key(no), no int not null auto_increment, reply int not null, time timestamp, name text, com text not null)")))

; html bits
(define (url-decode html-string)			; HACK
  (string-translate* html-string '(("%0D%0A" . "<br />")
				   ("%0D" . "<br />")
				   ("%21" . "!")
				   ("%22" . "\"")
				   ("%23" . "#")
				   ("%24" . "$")
				   ("%26" . "&")
				   ("%27" . "'")
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
  (string-substitute "&gt;&gt;([0-9]+)" "<a href='#r\\1'>&gt;&gt;\\1</a>" (string-substitute "^&gt;(?!&gt;)(.*?)<br />" "<span class='quote'>&gt;\\1</span><br />" (string-substitute "<br />&gt;(?!&gt;)(.*?)<br />" "<br /><span class='quote'>&gt;\\1</span><br />" text #t) #t) #t))

(define (fancytitle) (string-append "&lambda;::<a href='" self "'>" title "</a>"))

(define (make-logo reply)
  (div "logo" (string-append (tag-s "h1" (fancytitle)) (tag-s "h2" subtitle)
			     (a self "index") " / " (a "manage.scm" "manage") " / "
			     (if (eqv? reply 0)
			       (tag-s "span" "viewing index")
			       (tag-s "span" (string-append "viewing thread " (number->string reply)))))))

(define (wrap header body)
  (string-append "<!doctype html>" (tag-s "html" (string-append (tag-s "head" header) (tag-s "body" body)))))

(define (make-postform reply)
  (string-append "<form action='submit.scm' method='post'><label for='name'>name</label> <input type='text' name='name' /><input type='submit' value='Post' /><br /> <textarea name='com'></textarea><input type='hidden' name='reply' value='" (number->string reply) "' /></form>"))

(define (format-post row reply)
  (div-c "post" (string-append
		  "<a href='" (if (eq? reply 0)
				(string-append "?" (car row))
				(string-append "?" (number->string reply) "#r" (car row)))
		  "'>" (tag "span" (string-append "r" (car row)) "num" (car row)) "</a>"
		  " / "
		  (tag "span" #f "name" (if (mysql-null? (cadddr row))
					  defname
					  (url-decode (cadddr row))))
		  " "
		  (tag "span" #f "com" (apply-markup (url-decode (car (cddddr row))))))))

(define (make-post curs fetch reply)
  (define row (fetch))
  (if (not (or (mysql-null? row) (eqv? #f row)))
    (begin
      (define curs (string-append curs (format-post row reply)))
      (make-post curs fetch reply))
    curs))

(define (make-posts reply)
  (define fetch (con (string-append "select * from posts where no = '" (number->string reply) "'")))
  (define op (make-post "" fetch reply))
  (define fetch (con (string-append "select * from posts where reply = '" (number->string reply) "'")))
  (string-append op (make-post "" fetch reply)))

(define make-foot "- <a href='https://github.com/knarka/soykaf/'>soykaf</a> -")

(define (display-header)
  (display "Content-Type: text/html")
  (newline)
  (newline))

(define (display-page reply)
  (display
    (wrap 
      (string-append (tag-s "title" title) "<link rel='stylesheet' href='style.css' />")
      (string-append (make-logo reply) (div "postform" (make-postform reply)) (div "posts" (make-posts reply)) (div "foot" make-foot)))))

(define (make-arg-list query)
  (map (lambda (s)
	 (define arg (cdr (string-split s "=")))
	 (if (not (null? arg))
	   (car arg)
	   '()))
       (string-split query "&")))

(define (refresh) (display (string-append "<meta http-equiv='refresh' content='1;URL=" self "' />")))

(define (redirect page)
  (display (string-append "<meta http-equiv='refresh' content='0;URL=" page "' />")))

; prepare
(if (not (table-exists? "posts"))
  (table-create "posts"))

(define view (string->number (get-environment-variable "QUERY_STRING")))
(if (eqv? view #f) (define view 0))

; write pages
(if (eqv? (get-environment-variable "REQUEST_METHOD") #f) (exit 1)) ; not a cgi environment
(display-header)
(display-page view)
(newline)
