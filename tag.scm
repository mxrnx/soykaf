(define (tag t id class cont)
  (string-append "<" t 
		 (if (not (eqv? id #f)) (string-append " id='" id "'") "")
		 (if (not (eqv? class #f)) (string-append " class='" class "'") "")
		 ">" cont "</" t ">"))

(define (tag-s t cont) (tag t #f #f cont))
(define (div id cont) (tag "div" id #f cont))
(define (div-c class cont) (tag "div" #f class cont))
