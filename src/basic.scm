(module basic-lexer (trim string-literal dec-number char-literal identifier
                          inside-bracket comma-sep)
  (import chicken r5rs)
  (import lexer parser utils)

  (define (@ l x)
    (let loop ((xs l))
      (cond ((null? xs) (syntax-error "not found" x))
            ((equal? (caar xs) x) (cadar xs))
            (else (loop (cdr xs))))))

  (define basic (make-lexer #t "" "" "" '(#\\ #\") anychar anychar))

  (define trim (@ basic 'trim))
  (define string-literal (@ basic 'string-literal))
  (define dec-number (@ basic 'dec-number))
  (define char-literal (@ basic 'char-literal))
  (define identifier (@ basic 'identifier))
  (define inside-bracket (@ basic 'inside-bracket))
  (define comma-sep (@ basic 'comma-sep))
)
