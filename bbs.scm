#!/usr/bin/csi -s 

(load "conf.scm")
(load "tag.scm")

(if (string=? lang "en") (load "str/en.scm"))
(if (string=? lang "jp") (load "str/jp.scm"))

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

(define (table-create! table)
  (con (string-append "create table " table " (primary key(no), no int not null auto_increment, reply int not null, time timestamp, name text, com text not null, ip text not null)")))

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
			     (a self s-index) " / " (a "?manage" s-manage) " / "
			     (if (string? reply)
			       (tag-s "span" s-status-manage)
			       (if (eqv? reply 0)
			         (tag-s "span" s-status-index)
			         (tag-s "span" (string-append (string-substitute "NUM" (number->string reply) s-status-thread #t))))))))

(define (wrap header body)
  (string-append "<!doctype html>" (tag-s "html" (string-append (tag-s "head" header) (tag-s "body" body)))))

(define (make-postform reply)
  (string-append "<form action='submit.scm' method='post'><label for='name'>" s-name " <input type='text' name='name' /><input type='submit' value='" s-post "' /><br /> <textarea name='com'></textarea><input type='hidden' name='reply' value='" (number->string reply) "' /></form>"))

(define (format-post row reply)
  (if (string=? (number->string reply) (car row))
    (define c "post op")
    (define c "post reply"))
  (string-append
    (div-c c (string-append
	       "<a href='"
	       (if (eq? reply 0)
		 (string-append "?" (car row))
		 (string-append "?" (number->string reply) "#r" (car row)))
	       "'>" (tag "span" (string-append "r" (car row)) "num" (car row)) "</a>"
	       " / "
	       (tag "span" #f "name" (if (mysql-null? (cadddr row))
				       defname
				       (url-decode (cadddr row))))
	       " "
	       (tag "span" #f "com" (apply-markup (url-decode (car (cddddr row)))))))
    (if (string=? c "post op") "<hr class='ophr' />" "")))

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

(define (make-rm-button no)
  (string-append "<form action='rm.scm' method='post'><input type='hidden' name='no' value='" no "' /><input type='submit' value='" s-mm-rm "' /></form>"))

(define (format-manager-post row)
  (tag-s "tr" (string-append
		  (td (car row))
		  (td (cadr row))
		  (td (string-append (if (mysql-null? (cadddr row)) defname (url-decode (cadddr row)))
				     "<br />"
				     (cadr (cddddr row))))
		  (td (apply-markup (url-decode (car (cddddr row)))))
		  (tag "td" #f "mmrm" (make-rm-button (car row))))))

(define (make-manager-post curs fetch)
  (define row (fetch))
  (if (not (or (mysql-null? row) (eqv? #f row)))
    (begin
      (define curs (string-append curs (format-manager-post row)))
      (make-manager-post curs fetch))
    curs))

(define (make-manager-posts!)
  (define fetch (con (string-append "select * from posts order by no desc")))
  (tag-s "table" (string-append
		   (tag "tr" "toprow" #f (string-append (td s-mm-no) (td s-mm-re) (td s-mm-name) (td s-mm-comment) (td "")))
		   (make-manager-post "" fetch))))

(define make-foot "- <a href='https://github.com/knarka/soykaf/'>soykaf</a> -")

(define (make-arg-list query)
  (map (lambda (s)
	 (define arg (cdr (string-split s "=")))
	 (if (not (null? arg))
	   (car arg)
	   '()))
       (string-split query "&")))

; non-functional code follows
(define (display-header!)
  (display "Content-Type: text/html")
  (newline)
  (newline))

(define (valid? passwd) (string=? passwd (string-append "managepass=" adminpass)))

(define (display-manager!)
  (define passwd (get-environment-variable "HTTP_COOKIE"))
  (if (or (eq? passwd #f) (not (valid? passwd)))
    (define form "<form method='post' action='login.scm'><input type='password' name='managepass' /><input type='submit' value='log in' /></form>")
    (define form "<form method='post' action='login.scm'>logged in <input type='hidden' name='managepass' /><input type='submit' value='log out' /></form>"))
  (display
    (wrap 
      (string-append (tag-s "title" title) "<meta charset='utf-8' /><link rel='stylesheet' href='css/style.css' />")
      (string-append (make-logo "") (div "postform" form) (if (valid? passwd) (make-manager-posts!) "") (div "foot" make-foot)))))

(define (display-page! reply)
  (display
    (wrap 
      (string-append (tag-s "title" title) "<meta charset='utf-8' /><link rel='stylesheet' href='css/style.css' />")
      (string-append (make-logo reply) (div "postform" (make-postform reply)) (div "posts" (make-posts reply)) (div "foot" make-foot)))))

(define (refresh!) (display (string-append "<meta http-equiv='refresh' content='1;URL=" self "' />")))

(define (redirect! page)
  (display (string-append "<meta http-equiv='refresh' content='0;URL=" page "' />")))

; prepare
(if (not (table-exists? "posts"))
  (table-create! "posts"))

(if (eqv? (get-environment-variable "REQUEST_METHOD") #f) (exit 1)) ; not a cgi environment

(display-header!)

(define view (get-environment-variable "QUERY_STRING"))
(define numv (string->number view))
(if (eqv? numv #f) (define numv 0))
(if (string=? view "manage")
  (display-manager!)
  (display-page! numv))
(newline)
