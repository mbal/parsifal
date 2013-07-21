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
(define-syntax defparser
  (syntax-rules (&)
    ((defparser ((name (& altnames ...) params ...) state) body)
     (begin
       (define (name params ...)
         (lambda (state) body))
       (define altnames name) ...))
    ((defparser ((name params ...) state) body)
     (define (name params ...)
       (lambda (state) body)))))

;; successful if the first character of the input satisfies
;; the predicate. 
;; idea: satisfy accept an optional error message, on unexpected-input
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
;; If then is *really* equivalent to >>, then is associative, as it
;; follows from the monads laws.
(defparser ((then (& >>) p1 p2) state)
  (let ((result1 (p1 state)))
    (if (successful? result1)
      (let ((result2 (p2 result1)))
        (copy-state-except result2 empty? 
                           (and (empty? result2) (empty? result1))))
      result1)))

;; bind :: Parser a -> (a -> Parser b) -> Parser b
(defparser ((bind (& >>=) p1 f) state)
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
                      (reverse acc) (empty? state) #f))))))

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

;; simpler derived parsers
(define (char c) (satisfy (lambda (x) (char=? c x))))
(define letter (satisfy char-alphabetic?))
(define anychar (satisfy (constantly #t)))
(define digit (satisfy char-numeric?))

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
(define (str s)
  (foldr then (succeed s) (map-string char s)))

(define (parse P input)
  (P (make-state (string->list input) 0 '() #t #f)))

(define (run P input)
  (listify (value (parse P input))))

