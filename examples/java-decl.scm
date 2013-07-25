(module java-decl (run-java trim)
  (import chicken r5rs)
  (import parser lexer)

  (define (@ l x)
    (let loop ((xs l))
      (cond ((null? xs) (syntax-error "not found" x))
            ((equal? (caar xs) x) (cadar xs))
            (else (loop (cdr xs))))))

  (define java (make-lexer #t "//" "/*" "*/" '(#\\ #\") 
                           (either letter (char #\_))
                           (satisfy (lambda (x) (or (char-alphabetic? x)
                                                    (char-numeric? x))))))

  (define trim (@ java 'trim))
  (define string-literal (@ java 'string-literal))
  (define dec-number (@ java 'dec-number))
  (define char-literal (@ java 'char-literal))
  (define identifier (@ java 'identifier))
  (define inside-bracket (@ java 'inside-bracket))
  (define comma-sep (@ java 'comma-sep))

  (defparser (value type parser tag)
             (named-bind
               (str type)
               (name-value <- (comma-sep
                                (named-bind
                                  (name <- identifier)
                                  (value <- (opt (then (char #\=)
                                                       trim
                                                       parser)))
                                  (succeed (cons name value)))))
               (char #\;)
               (succeed (list name-value tag))))

  (define strd (value "string" string-literal 'string))
  (define intd (value "int" signed-number 'int))

  (define java-decl
    (then
      trim
      (either strd intd)))

  (define (run-java p)
    (parse java-decl p))

  )
