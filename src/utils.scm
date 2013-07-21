(module utils (map-string string-append1 listify reduce2)
  (import r5rs chicken)

  ;; map on strings. To me, this is one of the biggest problem in scheme:
  ;; the lack of genericity. LENGTH should work on every "multiple" data type,
  ;; lists, strings, vectors. As should MAP.
  (define (map-string f l)
    (map f (string->list l)))

  ;; append a char to a string, in first position
  (define (string-append1 c s)
    (list->string (cons c (string->list s))))

  ;; a 2-arity fold. The list should be at least 2 item long.
  ;; It applies f to the first to items, and then works like
  ;; a normal foldl
  (define (reduce2 f l)
    (foldl f (f (car l) (cadr l)) (cddr l)))

  (define (listify x) 
    (if (list? x) x (list x))))
