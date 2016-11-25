#!/usr/bin/csi -s 

;;; CONFIG STARTS HERE ;;;
(define title "shima bbs")		; the title of your board
(define subtitle "tiny scheme board")	; tagline displayed under the title

(define defname "anonymous")		; name to use when user doesn't fill one in

(define self "bbs.scm")			; name of this file
(define dbfile "shimadb")		; name of the posts file
;;; CONFIG ENDS HERE ;;;

;(define dbgraball
  ;(lambda ()
    ;(open-input-file dbfile)
    ;(let i 0)
    ;(if (< i ))

(define tag
  (lambda (t cont)
    (string-append "<" t ">" cont "</" t ">")))

(define encodingtag "<meta content='utf-8' />")

(define fancytitle (lambda () (string-append "&lambda;::<a href='" self "'>" title "</a>")))

(define wrap
  (lambda (header body)
    (string-append "<!doctype html>" (tag "html" (string-append (tag "head" header) (tag "body" body))))))

(define displayheader
  (lambda ()
    (display "Content-Type: text/html")
    (newline)
    (newline)))

(define displaypage
  (lambda ()
    (display
      (wrap 
	(string-append (tag "title" title) encodingtag)
	(string-append (tag "h1" (fancytitle)) (tag "h2" subtitle))))))

(define displayfooter 0)

(displayheader)
(displaypage)
(newline)
