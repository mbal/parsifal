(module lexer (make-parser deflexer)
  (import chicken r5rs)
  (import parser utils)

  (define new-line (char #\newline))

  (define escape-error "invalid escape sequence")

  (define-syntax deflexer
    (syntax-rules ()
      ((deflexer lex (def name body) ...)
       (define lex 
         (begin 
           (def name body) ...
           (list `(name ,name) ...))))))

  (deflexer (make-parser 
              comment-line-start
              comment-block-start
              comment-block-end
              escapable-characters
              identifier-start
              identifier-letter)

    (defparser block-comment
               (skip (between (str comment-block-start) 
                              (str comment-block-end) 
                              anychar)))

    (defparser line-comment 
               (skip
                 (str comment-line-start)
                 (many-until anychar (either new-line eof))))

    (define escape (one-of escapable-characters))

    (defparser char-literal
               (either
                 (<?> (then (char #\\) escape) escape-error)
                 (satisfy (lambda (x) (and (not (char=? x #\"))
                                           (not (char=? x #\\)))))))
    (defparser identifier 
               (stringify 
                 (then identifier-start (many identifier-letter))))

    (defparser dec-number
               (named-bind
                 (s <- (opt (one-of '(#\+ #\-)) #\+))
                 (n <- (many1 digit))
                 (succeed (list->number (cons s n)))))

    (defparser string-literal
               (stringify (between (char #\") (char #\") char-literal)))
    ))
