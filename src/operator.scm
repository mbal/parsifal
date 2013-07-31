(module operator (add-op mul-op exp-op
                         add-op-s mul-op-s exp-op-s and-op or-op and-op-s
                         or-op-s)
  (import r5rs chicken)
  (import parser)

  ;; this module provides parser for common operators, both "functional" variant
  ;; and "symbolic".
  ;; Functional variant means that the parser succeeds with a function that
  ;; can be executed on the values, while symbolic means that it returns a
  ;; symbol, that can be used for e.g. creating trees of expressions.

  (define add-op
    (named-bind
     (o <- (one-of '(#\+ #\-)))
     (succeed (if (equal? #\+ o) + -))))

  (define mul-op
    (named-bind
     (o <- (one-of '(#\* #\/ #\%)))
     (succeed (cond ((equal? o #\*) *)
                    ((equal? o #\/) /)
                    (else remainder)))))

  (define and-op
    (then (str "&&") (lambda (a b) (and a b))))

  (define or-op
    (then (str "||") (lambda (a b) (or a b))))

  (define exp-op
    (then (char #\^)
          (succeed expt)))

  ;; Symbolic parsers ------------------------------ 
  ;; Same as the parsers above, but, instead of applying the operation, they
  ;; return a list of (op a b), useful to create a parse tree.
  ;;

  (define add-op-s
    (named-bind
     (o <- (one-of '(#\+ #\-)))
     (succeed (if (equal? #\+ o)
                  (lambda (a b) (list '+ a b))
                  (lambda (a b) (list '- a b))))))

  (define mul-op-s
    (named-bind
     (o <- (one-of '(#\* #\/ #\%)))
     (succeed (cond ((equal? o #\*) (lambda (a b) (list '* a b)))
                    ((equal? o #\/) (lambda (a b) (list '/ a b)))
                    (else (lambda (a b) (list 'remainder a b)))))))

  (define and-op-s
    (then (str "&&") (lambda (a b) (list 'and a b))))

  (define or-op-s
    (then (str "||") (lambda (a b) (list 'or a b))))

  (define exp-op-s
    (then (char #\^)
          (succeed (lambda (a b) (list 'expt a b)))))
  )
