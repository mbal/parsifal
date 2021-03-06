(module parse-test (run-all)
  (import r5rs chicken parser state)

  (define (basic-tests)
    (defparser simple-parser (char #\a))
    (assert (successful? (parse simple-parser "a")))
    (assert (successful? (parse simple-parser "aa")))
    (assert (equal? (value (parse simple-parser "a")) #\a))
    (assert (not (successful? (parse simple-parser "b")))))

  (define (anychar-test)
    (assert (successful? (parse anychar "0")))
    (assert (successful? (parse anychar "a")))
    (assert (not (successful? (parse anychar "")))))

  (define (digit-test)
    (assert (successful? (parse digit "0")))
    (assert (successful? (parse digit "9")))
    (assert (not (successful? (parse digit "a")))))

  (define (letter-test)
    (assert (successful? (parse letter "a")))
    (assert (successful? (parse letter "A")))
    (assert (not (successful? (parse letter "9")))))

  (define (one-of-test)
    (assert (successful? (parse (one-of '(#\a #\0)) "a")))
    (assert (successful? (parse (one-of '(#\a #\0)) "0")))
    (assert (not (successful? (parse (one-of '(#\a #\0)) "1")))))

  (define (eof-test)
    (assert (successful? (parse eof "")))
    (assert (not (successful? (parse eof "a")))))

  (define (str-test)
    (assert (successful? (parse (str "string") "string")))
    (assert (equal? (value (parse (str "string") "string")) "string"))
    (assert (not (successful? (parse (str "strong") "string"))))
    (assert (not (successful? (parse (str "strong") "weak")))))

  (define (word-test)
    (assert (successful? (parse word "abc ")))
    (assert (equal? (value (parse word "abc ")) "abc"))
    (assert (not (successful? (parse word "1abc ")))))

  (define (run-all)
    (basic-tests)
    (word-test)
    (str-test)
    (eof-test)
    (one-of-test)
    (letter-test)
    (digit-test)
    (anychar-test))
  )
