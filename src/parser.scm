(module parser
  (many1 many sep-by sep-by1 either then bind word run str try succeed digit
         anychar parse char one-of skip-many skip-many1 named-bind <?> eof
         number defparser >> >>= letter between many-until opt stringify skip
         satisfy none-of after signed-number chainr1 chainr chainl1 chainl) 


  (import chicken r5rs data-structures)
  (import utils state)

  (define (unexpected-input in state)
    (make-state (input state) (position state)
                (value state) #t (list "unexpected" in)))

  (define (unexpected sym state)
    (make-state (input state) (position state)
                (value state) #t (list "unexpected" sym)))

  (define (eof? x) (equal? x '()))

  ;; allows to define a combinator and defines alternatives names for the
  ;; function (using the symbol &). A combinator is a (partially curried)
  ;; function that takes one or more parsers, and a state.
  ;;
  ;; (defcomb ((name args) state) body)
  ;; is equivalent to 
  ;; (define (name args) (lambda (state) body))
  ;;
  ;; USAGE:
  ;; (defcomb ((NAME (& OTHERNAMES) P1 P2) STATE)
  ;;    BODY)
  ;;
  ;; The whole (& OTHERNAMES) is optional.
  ;; It's possible to define a parser through the . notation, which means
  ;; that the parser will `reduce' (fold) the body of the function over the 
  ;; list of parameters.
  ;; For example, (>> a b c d e) == (reduce2 >> (a b c d e)) ==
  ;;              (>> (>> (>> (>> a b) c) d) e)
  (define-syntax defcomb
    (syntax-rules (&)
      ((defcomb ((name (& altname altnames ...) params ...) state) body)
       (begin
         (defcomb ((name params ...) state) body)
         (define altname name)
         (define altnames name) ...))
      ((defcomb ((name (& altname altnames ...) p q . rest) state) body)
       (begin
         (defcomb ((name p q . rest) state) body)
         (define altname name)
         (define altnames name) ...))
      ((defcomb ((name params ...) state) body)
       (define (name params ...)
         (lambda (state) body)))
      ((defcomb ((name p q . rest) state) body)
       (define name
         (case-lambda
           ((p q) (lambda (state) body))
           ((p q . rest)
            (reduce2 name (append (list p q) rest))))))))

  ;;; transforms this:
  ;;; (bind anychar (lambda (c) (char c))) into this:
  ;;; (named-bind (c <- anychar) (char c))
  (define-syntax named-bind
    (syntax-rules (<-)
      ((named-bind (x <- parser) body)
       (bind parser (lambda (x) body)))
      ((named-bind (x1 <- parser1) rest ...)
       (bind parser1 (lambda (x1) (named-bind rest ...))))
      ((named-bind parser body)
       (then parser body))
      ((named-bind parser body ...)
       (then parser (named-bind body ...)))))

  ;; defines a parser. 
  ;; Wraps the body of the parser with a lambda expression, so you can
  ;; emulate lazyness without explicitly writing the closure.
  (define-syntax defparser
    (syntax-rules ()
      ((defparser (name args) (body ...))
       (define (name args)
         (lambda (state)
           ((body ...) state))))
      ((defparser name (body ...))
       (define name
         (lambda (state)
           ((body ...) state))))))

  ;; runs p and, if not successful, returns the specified error.
  (defcomb ((<?> p err) state)
           (let ((result (p state)))
             (if (successful? result)
               result
               (copy-state-except result error (list err)))))

  ;; successful if the first character of the input satisfies
  ;; the predicate. 
  ;; satisfy :: (Char -> Bool) -> Parser Char
  (defcomb ((satisfy pred) state)
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
  (defcomb ((then (& >>) p1 p2 . more) state)
           (let ((result1 (p1 state)))
             (if (successful? result1)
               (let ((result2 (p2 result1)))
                 (copy-state-except result2 empty? 
                                    (and (empty? result2) (empty? result1))))
               result1)))

  (defparser (after p1 p2)
             (named-bind
               (x <- p1)
               p2
               (succeed x)))

  (defcomb ((between bra ket inside) state)
           ;; yeah, pretty ugly. It's **almost** equivalent to
           ;; (named-bind bra (x <- (many inside)) ket (succeed x)), but:
           ;; (parse (between (char #\a) (char #\b) letter) "ajkbcdeb")
           ;; returns "jk", while the named-bind version raises an
           ;; unexpected EOF error. The named bind version is good enough for
           ;; e.g. comments in programming languages, since there are two
           ;; delimiters (e.g # and newline) that cannot appear anywhere
           ;; in the comment. Is it good enough for the general case?
           (let ((start (bra state)))
             (if (successful? start)
               (let loop ((prev start) (parsed '()))
                 (let ((end (ket prev)))
                   (if (successful? end)
                     (copy-state-except end value (reverse parsed))
                     (let ((middle (inside prev)))
                       (if (successful? middle)
                         (loop middle (cons (value middle) parsed))
                         middle)))))
               (if (eof? (input state))
                 (unexpected "EOF" state)
                 (unexpected-input (car (input state)) state)))))

  ;; this is the return in the monad.
  (defcomb ((succeed v) state)
           (copy-state-except state value v))

  ;; bind :: Parser a -> (a -> Parser b) -> Parser b
  (defcomb ((bind (& >>=) p1 f) state)
           (let ((result1 (p1 state)))
             (if (successful? result1)
               (let ((result2 ((f (value result1)) result1)))
                 (copy-state-except result2 empty?
                                    (and (empty? result2) (empty? result1))))
               result1)))

  ;; runs p1, and, if successful, returns. If not not successful and
  ;; p1 didn't consume any input (i.e. it failed at the first match)
  ;; runs p2
  (defcomb ((either (& <or>) p1 p2 . more) state)
           (let ((result1 (p1 state)))
             (if (and (not (successful? result1)) (empty? result1))
               (p2 state)
               result1)))

  ;; kleene star operator.
  ;; many :: Parser a -> Parser [a]
  ;; basically, it allows to transform character parsers to string parsers.
  ;; equivalent to:
  ;; (either
  ;;    (named-bind (x <- p) (xs <- (many p)) (succeed (cons x xs)))
  ;;    (succeed '()))
  (defcomb ((many p) state)
           (let loop ((s state) (acc '()))
             (let ((result (p s)))
               (if (and (successful? result) (not (empty? result)))
                 (loop result (cons (value result) acc))
                 (if (empty? result)
                   (make-state (input result) (position result)
                               (reverse acc) (empty? state) #f)
                   result)))))

  ;; kleene plus
  (defparser (many1 p)
             (named-bind
               (x <- p)
               (y <- (many p))
               (succeed (cons x y))))

  (defparser (skip p) (then p (succeed '())))

  (defparser (skip-many p)
             (either
               (then
                 p
                 (skip-many p))
               (succeed '())))

  (defparser (skip-many1 p)
             (then
               p
               (skip-many p)))

  ;; (try p) behaves like p, but, on failing, it pretends that nothing
  ;; happened (i.e. that the parser hasn't read any character)
  (defcomb ((try p) state)
           (let ((result (p state)))
             (if (successful? result)
               result
               (make-state (input state) (position state) (value state)
                           #t (error result)))))

  (define (sep-by1 p sep)
    (named-bind
      (x <- p)
      (xs <- (many (then sep p)))
      (succeed (cons x xs))))

  (define (sep-by p sep)
    (either (sep-by1 p sep)
            (succeed '())))

  ;; parses p, and, as long as there is a binary operation op, reads it and
  ;; another p, then applies `perform` on the two values. The operator
  ;; should associate to the left
  (define (chainl1 p op perform)
    (begin
      (define (func prev) 
        (either (named-bind op (b <- p) (func (perform prev b)))
                (succeed prev)))
      (named-bind
        (a <- p)
        (in <- (func a))
        (succeed in))))

  ;; as chainl1, but returns a default if there's no initial match
  (define (chainl p op perform default)
    (either (chainl1 p op perform) (succeed default)))

  ;; accumulates p to the right. While it parses the sequence op p, it 
  ;; traverse the string. When there are no more op, it rewinds itself,
  ;; folding back the results of p, applying `perform`.
  (define (chainr1 p op perform)
    (named-bind
      (a <- p)
      (either
        (named-bind
          op
          (b <- (chainr1 p op perform))
          (succeed (perform a b)))
        (succeed a))))

  (define (chainr p op perform default)
    (either (chainr1 p op perform) (succeed default)))

  ;; simpler derived parsers
  (define (char c) (satisfy (lambda (x) (char=? c x))))
  (define letter (satisfy char-alphabetic?))
  (define anychar (satisfy (constantly #t)))
  (define digit (satisfy char-numeric?))
  (define (one-of l) (satisfy (lambda (x) (member x l))))
  (define (none-of l) (satisfy (lambda (x) (not (member x l)))))

  (define (opt p #!optional (default '()))
    (either p (succeed default)))

  (define number 
    (<?> (lambda (state)
           (let ((result ((many1 digit) state)))
             (if (successful? result)
               (copy-state-except result value (list->number (value result)))
               result)))
         "expected number"))

  (defparser signed-number
             (named-bind
               (s <- (opt (either (char #\+) (char #\-)) #\+))
               (n <- number)
               (succeed (if (char=? #\+ s) n (- n)))))

  (define (word state)
    (let ((result ((many1 letter) state)))
      (if (successful? result)
        (copy-state-except result value (list->string (value result)))
        result)))
  ;; do you want a word, right? Let's get back the result to a string

  ;; this version is very general, and, in fact, in Haskell the whole
  ;; thing is called mapM. That this is hardly an improvement over 
  ;; the standard recursive version. The only good thing is the
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

  (defparser (many-until repeat limit)
             (either
               (then limit (succeed '()))
               (named-bind 
                 (x <- repeat)
                 (xs <- (many-until repeat limit))
                 (succeed (cons x xs)))))


  ;; matches if the input ends at the position
  (define (eof state)
    (if (eof? (input state))
      state
      (unexpected-input (car (input state)) state)))

  (defparser (stringify p)
             (named-bind
               (x <- p)
               (succeed (list->string x))))


  ;; ------------------------------------

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
