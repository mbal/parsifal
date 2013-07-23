(module csv (run-csv)
  (import r5rs chicken)
  (import parser state)

  ;; A CSV file is either
  ;; - empty
  ;; - one or more lines
  ;;
  ;; A line consists in words, separated by ,
  ;; so:

  (defparser csv%
       (either
         (many1 (sep-by1 word (char #\,)) (char #\newline))
         (succeed '())))

  ;; (run csv% "a,b,c,d") ==> (("a" "b" "c" "d"))
  ;; This parser works fine, but it just parses the first line

  (defparser new-line (char #\newline))
  (defparser csv%%
        (either
          (many1
            (then
              (sep-by1 word (char #\,))
              new-line))
          (succeed '())))

  ;; but this doesn't work, for two reasons:
  ;;  - the file has to end with a newline
  ;;  - we don't save the parsed line in the result

  (defparser csv-line (sep-by1 word (one-of '(#\, #\;))))

  (defparser csv (either
                   (sep-by1 (sep-by1 word (char #\,)) 
                            (many1 new-line))
                   (succeed '())))

  (define (run-csv str)
    (parse csv str))
  )
