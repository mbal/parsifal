(defparser (line-comment s)
  (then
    (str s)
    (many-until anychar (either new-line eof))
    (succeed '())))

(define new-line (char #\newline))

(define escape-error "invalid escape sequence")

(defparser (block-comment bstart bend)
           (skip (between (str bstart) (str bend) anychar)))

(define escape (one-of '(#\" #\\)))

(defparser char-literal
           (either
             (<?> (then (char #\\) escape) escape-error)
             (satisfy (lambda (x) (and (not (char=? x #\"))
                                       (not (char=? x #\\)))))))

(defparser string-literal
           (stringify (between (char #\") (char #\") char-literal)))

