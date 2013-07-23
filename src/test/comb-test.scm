(module comb-test (run-all)
  (import r5rs chicken parser state) 

  (define (test-then)
    (define open-close (then (char #\[) (char #\])))
    (assert (successful? (parse open-close "[]")))
    (assert (successful? (parse open-close "[])")))
    (assert (not (successful? (parse open-close "[[]"))))
    (assert (not (successful? (parse open-close "[t]")))))

  (define (test-either)
    (define alt-parens (either (char #\() (char #\))))
    (assert (successful? (parse alt-parens "(")))
    (assert (successful? (parse alt-parens ")")))
    (assert (not (successful? (parse alt-parens "a(")))))

  (define (test-either-then)
    (defparser parens
               (either 
                 (then
                   (char #\()
                   parens
                   (char #\))
                   parens)
                 (succeed '())))

    (assert (successful? (parse parens "()")))
    (assert (successful? (parse parens "(((())))")))
    ;; succeed doesn't mark the end of the string, so this matches
    (assert (successful? (parse parens "())"))) 
    (assert (successful? (parse parens "(())()"))) 
    (assert (not (successful? (parse parens "(()()")))))

  (define (test-either-then2)
    (define aorb-bogus
      (either (then (char #\[) (char #\a))
              (then (char #\[) (char #\b))))
    (assert (successful? (parse aorb-bogus "[a")))
    (assert (not (successful? (parse aorb-bogus "[b")))))

  (define (test-either-then-try)
    ;; of course, we could even modify the grammar to make it right
    (defparser aorb-right
      (either (try (then (char #\[) (char #\a)))
              (then (char #\[) (char #\b))))

    (assert (successful? (parse aorb-right "[b")))
    (assert (successful? (parse aorb-right "[a"))))

  (define (test-bind)
    (define parensbind
      (either
        (then (char #\()
              (bind (lambda (x) (parensbind x))
                    (lambda (n) 
                      (then 
                        (char #\))
                        (bind (lambda (x) (parensbind x))
                              (lambda (m) (succeed (max m (+ n 1)))))))))
        (succeed 0)))

    (assert (= 3 (value (parse parensbind "((()))"))))
    (assert (= 3 (value (parse parensbind "()((()))"))))
    (assert (= 1 (value (parse parensbind "()"))))
    (assert (not (successful? (parse parensbind "(()")))))

  (define (test-many)
    (assert (successful? (parse (many digit) "0912a")))
    ;; many accept even 0 occourrences
    (assert (successful? (parse (many digit) "aaa")))
    (assert (successful? (parse (many1 digit) "0912a")))
    (assert (successful? (parse (many1 digit) "1aa2")))
    ;; but many1 does not
    (assert (not (successful? (parse (many1 digit) "aaa"))))
    (assert (not (successful? (parse (many1 digit) "a90"))))
    (assert (equal? '(#\1) (value (parse (many1 digit) "1aa2"))))
    (assert (equal? (string->list "098") (value (parse (many1 digit) "098a")))))

  (define (test-sep-by)
    (assert (equal? '("hi" "di" "do") 
                    (run (sep-by1 word (char #\,)) "hi,di,do")))

    (assert (successful? (parse (sep-by word (char #\,)) "")))
    (assert (not (successful? (parse (sep-by1 word (char #\,)) ""))))

    (assert (not (successful? (parse (sep-by word (char #\,)) "hi,hi,ho,"))))
    (assert (not (successful? (parse (sep-by1 word (char #\,)) "hi,hi,ho,")))))

  (define (run-all)
    (test-sep-by)
    (test-either)
    (test-either-then)
    (test-either-then2)
    (test-either-then-try)
    (test-bind)
    (test-many)
    (test-sep-by))
  )
