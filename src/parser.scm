(module parser
  (successful? value error many1 many sep-by sep-by1 either then bind
               word run str try succeed digit anychar parse char
               one-of skip-many skip-many1 hask-do let*-bind)

  (import chicken r5rs utils data-structures)
;; first of all, what is a combinator?
;; a combinator is a higher-order function that has not a free variable in it
;;
(define (successful? state) (equal? (error state) #f))

(define (make-state input pos value empty? err)
  (list input pos value empty? err))

(define (copy-state-except original field new-value)
  (apply make-state
         (map (lambda (x) 
                (if (not (equal? field x)) 
                  (x original) 
                  new-value))
              (list input position value empty? error))))

(define input car)
(define position cadr)
(define value caddr)
(define empty? cadddr)
(define (error x) (car (cddddr x)))

(define (eof? x) (equal? x '()))

(define (unexpected-input in state)
  (make-state (input state) (position state)
              (value state) #t (list "unexpected" in)))

(define (unexpected sym state)
  (make-state (input state) (position state)
              (value state) #t (list "unexpected" sym)))

;; allows to define parser combinators and defines alternatives names for the
;; function (using the symbol &)
;;
;; USAGE:
;; (defparser ((NAME (& OTHERNAMES) P1 P2) STATE)
;;    BODY)
;; The whole (& OTHERNAMES) is optional.
;; It's possible to define a parser through the . notation, which means
;; that the parser will `reduce' (fold) the body of the function over the 
;; list of parameters.
;; For example, (>> a b c d e) == (fold >> (a b c d e)) ==
;;              (>> (>> (>> (>> a b) c) d) e)
(define-syntax defparser
  (syntax-rules (&)
    ((defparser ((name (& altname altnames ...) params ...) state) body)
     (begin
       (defparser ((name params ...) state) body)
       (define altname name)
       (define altnames name) ...))
    ((defparser ((name (& altname altnames ...) p q . rest) state) body)
     (begin
       (defparser ((name p q . rest) state) body)
       (define altname name)
       (define altnames name) ...))
    ((defparser ((name params ...) state) body)
     (define (name params ...)
       (lambda (state) body)))
    ((defparser ((name p q . rest) state) body)
     (define name
       (case-lambda
         ((p q) (lambda (state) body))
         ((p q . rest)
          (reduce2 name (append (list p q) rest))))))))

(define-syntax let*-bind
  (syntax-rules (_)
    ((let*-bind ((_ y)) body)
     (then y body))
    ((let*-bind ((_ y) whatever ...) body)
     (then y (let*-bind (whatever ...) body)))
    ((let*-bind ((x y)) body)
     (bind y (lambda (x) body)))
    ((let*-bind ((x y) (x2 y2) ...) body)
     (bind y (lambda (x) (let*-bind ((x2 y2) ...) body))))))

(define-syntax hask-do
  (syntax-rules (<-)
    ((hask-do (x <- parser) body)
     (bind parser (lambda (x) body)))
    ((hask-do (x1 <- parser1) rest ...)
     (bind parser1 (lambda (x1) (hask-do rest ...))))
    ((hask-do parser body ...)
     (then parser (hask-do body ...)))))

;; successful if the first character of the input satisfies
;; the predicate. 
;; satisfy :: (Char -> Bool) -> Parser Char
(defparser ((satisfy pred) state)
  (let ((data (input state)))
    (if (eof? data)
      (unexpected "EOF" state)
      (if (pred (car data))
        (make-state (cdr data) (+ 1 (position state))
                    (car data) #f #f)
        (unexpected-input (car data) state)))))

;; runs p1 and, if successful, runs p2.
;; is the then combinator associative?
;; EQUIVALENT TO haskell's >> (and to p1 >>= \x . p2)
;; (define then (bind p1 (lambda (x) p2)))
;; Is associative, as it follows from the monads laws.
(defparser ((then (& >>) p1 p2 . more) state)
  (let ((result1 (p1 state)))
    (if (successful? result1)
      (let ((result2 (p2 result1)))
        (copy-state-except result2 empty? 
                           (and (empty? result2) (empty? result1))))
      result1)))

;; bind :: Parser a -> (a -> Parser b) -> Parser b
(defparser ((bind (& >>=) p1 f . more) state)
  (let ((result1 (p1 state)))
    (if (successful? result1)
      (let ((result2 ((f (value result1)) result1)))
        (copy-state-except result2 empty?
                           (and (empty? result2) (empty? result1))))
      result1)))

;; runs p1, and, if successful, returns. If not not successful and
;; p1 didn't consume any input (i.e. it failed at the first match)
;; runs p2
(defparser ((either (& <or>) p1 p2) state)
  (let ((result1 (p1 state)))
    (if (and (not (successful? result1)) (empty? result1))
      (p2 state)
      result1)))

;; kleene star operator.
;; many :: Parser a -> Parser [a]
;; basically, it allows to transform character parsers to string parsers.
(defparser ((many p) state)
  (let loop ((s state) (acc '()))
    (let ((result (p s)))
      (if (and (successful? result) (not (empty? result)))
        (loop result (cons (value result) acc))
        (if (empty? result)
          (make-state (input result) (position result)
                      (reverse acc) (empty? state) #f)
          result)))))

;; kleene plus
(defparser ((many1 p) state)
  ;; yep. But I don't like it very much
  ;;  (bind p (lambda (x) 
  ;;            (bind (many p)
  ;;                  (lambda (y) 
  ;;                    (succeed (foldr cons (list x) y))))))
  (let ((result (p state)))
    (if (successful? result)
      (let ((result2 ((many p) result)))
        (copy-state-except result2 value
                           (cons (value result) (value result2))))
      result)))

;; this parser is recursive, and we are in a strict language. We wrap
;; the code in a lambda, otherwise, it will never stop.
(defparser ((skip-many p) state) 
  ((either 
    (then p (skip-many p))
    (succeed '())) state))

(define (skip-many1 p)
  (then
    p
    (skip-many p)))

;; (try p) behaves like p, but, on failing, it pretends that nothing
;; happened.
(defparser ((try p) state)
  (let ((result (p state)))
    (if (successful? result)
      result
      state)))

;; this is the return in the monad.
(defparser ((succeed v) state)
  (copy-state-except state value v))

(define (sep-by1 p sep)
  (let*-bind ((x p) (xs (many (then sep p))))
             (succeed (cons x xs))))

;(define (sep-by1 p sep)
;  (let*-bind ((x p) (xs (many (then sep p))))
;    (succeed x xs)))

;(define (sep-by1 p sep)
;  (let*-bind
;    (x  <- p)
;    (xs <- (many (then sep p)))
;    (succeed (cons x xs))))

(define (sep-by p sep)
  (either (sep-by1 p sep)
          (succeed '())))

;; simpler derived parsers
(define (char c) (satisfy (lambda (x) (char=? c x))))
(define letter (satisfy char-alphabetic?))
(define anychar (satisfy (constantly #t)))
(define digit (satisfy char-numeric?))
(define (one-of l) (satisfy (lambda (x) (member x l))))

(define (word state)
  (let ((result ((many1 letter) state)))
    ;; do you want a word, right? Let's get back the result to a string
    (copy-state-except result value (list->string (value result)))))

;; this version is very general, and, in fact, in Haskell the whole
;; thing is called mapM. We agree, however, that this is hardly an
;; improvement over the previous version. The only good thing is the
;; lack of explicit recursion, but since we are schemers, we don't
;; care about it very much
;; (foldr (lambda (m n)
;;           (bind
;;             (lambda (s1) (m s1))
;;             (lambda (x) 
;;               (bind (lambda (s1) (n s1))
;;                     (lambda (xs)
;;                       (succeed (string-append1 x xs)))))))
;;         (succeed "") (map-string char s)))
;; The following version is more or less equivalent, and should be
;; faster (but not as elegant as the Haskell mapM version)
(define (str s)
  (foldr then (succeed s) (map-string char s)))

(define (parse P input)
  (P (make-state (string->list input) 0 '() #t #f)))

(define (prettify x)
  (define (loop xs acc)
    (cond ((null? xs) (reverse acc))
          ((list? (car xs)) 
           (loop (cdr xs) (cons (loop (car xs)) acc)))
          ((string? (car xs)) 
           (loop (cdr xs) (cons (car xs) acc)))
          ((char? (car xs)) 
           (loop (cdr xs) (cons (string (car xs)) acc)))
          ((number? (car x)) 
           (loop (cdr xs) (cons (number->string (car xs)) acc)))
        (else 
          (loop (cdr xs) acc))))
  (apply string-append (intersperse (loop x '()) " ")))

(define (run P input)
  (let ((result (parse P input)))
    (if (successful? result)
      (listify (value result))
      (prettify (error result)))))
)
