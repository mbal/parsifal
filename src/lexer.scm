(module lexer (export-definitions make-lexer)
  (import chicken r5rs)
  (import parser utils)

  (define escape-error "invalid escape sequence")
  (define white-space (either (char #\space) (char #\newline)))
  (define new-line (char #\newline))


  ;; defines a closure and returns a list of all the definitions in the
  ;; closure, in a list of 'name function pairs.
  (define-syntax export-definitions
    (syntax-rules ()
      ((export-definitions lex (def name body) ...)
       (define lex
         (begin
           (def name body) ...
           (list `(name ,name) ...))))))

  (define (make-lexer skip-ws comment-line-start
                      comment-block-start comment-block-end
                      escapable-characters identifier-start
                      identifier-letter) 

    (begin
      (defparser block-comment 
                 (between (str comment-block-start) 
                          (str comment-block-end) 
                          anychar))

      (defparser line-comment 
                 (then (str comment-line-start)
                       (many-until anychar (either new-line eof))))

      (define escape (one-of escapable-characters))

      (define trim
        (let* ((line? (not (string=? comment-line-start "")))
               (block? (not (string=? comment-block-start ""))))
          (cond ((and line? block? skip-ws)
                 (skip-many (either (try line-comment) block-comment white-space)))
                ((and line? skip-ws)
                 (skip-many (either line-comment white-space)))
                ((and block? skip-ws)
                 (skip-many (either block-comment white-space)))
                (else (skip-many white-space)))))

      (defparser char-literal
                 (either
                   (<?> (then (char #\\) escape) escape-error)
                   (satisfy (lambda (x) (and (not (char=? x #\"))
                                             (not (char=? x #\\)))))))

      (defparser identifier 
                 (after
                   (stringify 
                     (named-bind
                       (x <- identifier-start)
                       (xs <- (many identifier-letter))
                       (succeed (cons x xs))))
                   trim))

      (defparser dec-number
                 (after
                   (named-bind
                     (s <- (opt (one-of '(#\+ #\-)) #\+))
                     (n <- (many1 digit))
                     (succeed (list->number (cons s n))))
                   trim))

      (defparser string-literal
                 (after
                   (stringify (between (char #\") (char #\") char-literal))
                   trim))

      ;; run the parser p inside the brackets.
      ;; Trims all the spaces after bopen before running p
      ;; and trims spaces afterwards.
      (defparser (inside-bracket bopen bclose p)
                 (between bopen bclose
                          (named-bind trim (x <- p) trim (succeed x))))

      ;; runs the parser p, then looks for a comma, and repeat the process.
      ;; trims both before and after comma.
      (defparser (comma-sep p)
                 (sep-by (named-bind 
                           trim 
                           (x <- p) 
                           trim
                           (succeed x)) (char #\,)))



      (list `(string-literal ,string-literal)
            `(dec-number ,dec-number)
            `(identifier ,identifier)
            `(char-literal ,char-literal)
            `(trim ,trim)
            `(comma-sep ,comma-sep)
            `(inside-bracket ,inside-bracket)
            `(line-comment ,line-comment)
            `(block-comment ,block-comment))
      ))
  )
