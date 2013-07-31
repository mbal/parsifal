(module operator (add-op mul-op exp-op
                         add-op-s mul-op-s exp-op-s)
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
      (succeed (if (equal? #\+ o)
                 (lambda (a b) (+ a b))
                 (lambda (a b) (- a b))))))

  (define mul-op
    (named-bind
      (o <- (one-of '(#\* #\/ #\%)))
      (succeed (cond ((equal? o #\*) (lambda (a b) (* a b)))
                     ((equal? o #\/) (lambda (a b) (/ a b)))
                     (else (lambda (a b) (remainder a b)))))))

  (define exp-op
    (then (char #\^)
          (succeed (lambda (a b) (expt a b)))))

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
  (define exp-op-s
    (then (char #\^)
          (succeed (lambda (a b) (list 'expt a b)))))
)
