(define (map-string f l)
  (map f (string->list l)))

(define (string-append1 c s)
  (list->string (cons c (string->list s))))

(define (listify x) 
  (if (list? x) x (list x)))
