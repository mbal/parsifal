(module operator (add-op mul-op)
  (import r5rs chicken)
  (import parser)

  ;; this module provides parser for common operators, both "functional" variant
  ;; and "symbolic".
  ;; Functional variant means that the parser succeeds with a function that
  ;; can be executed on the values, while symbolic means that it returns a
  ;; symbol, that can be used for e.g. creating trees of expressions.

  (define-syntax parse-tokens
    (syntax-rules (=>)
      ((parse-tokens "grouped" ((c => return) ...))
       (lambda (x) (cond ((equal? x c) return) ...)))))

  (define-syntax group3
    (syntax-rules ()
      ((group3 "1" acc ()) (parse-tokens "grouped" acc))
      ((group3 "2" acc ()) (parse-tokens "grouped" acc))
      ((group3 "1" (a b c rest ...))
       (group3 "2" (a b c) (rest ...)))
      ((group3 "2" prev (a b c rest ...))
       (group3 "2" (prev . ((a b c))) (rest ...)))
      ((group3 a b c other ...)
       (group3 "1" (a b c other ...)))))

  (define-syntax create-op-parser
    (syntax-rules (=>)
      ((create-op-parser name parser tok-return ...)
       (define name
         (named-bind
           (o <- parser)
           (succeed ((group3 tok-return ...) o)))))))

  (create-op-parser add-op (one-of '(#\+ #\-))
                    #\+ => (lambda (a b) (+ a b))
                    #\- => (lambda (a b) (- a b)))

  (create-op-parser mul-op (one-of '(#\* #\/))
                    #\* => (lambda (a b) (* a b)) 
                    #\/ => (lambda (a b) (/ a b)))
)
