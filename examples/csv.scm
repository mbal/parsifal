(module csv (run-csv csv-line csv)
  (import r5rs chicken)
  (import parser state)

  ;; this parser is not really as simple as the other examples, I'll break it
  ;; down.
  (defparser csv
             (named-bind
               (n <- csv-line)
               (f <- (either
                       (named-bind 
                         new-line
                         (ns <- csv)
                         (succeed ns))
                       (succeed '())))
               (succeed (cons n f))))

  (defparser new-line (char #\newline))

  (define (run-csv f)
    (parse csv f))
  )
