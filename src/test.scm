(define simple-parser (char #\a))
(assert (successful? (parse simple-parser "a")))
(assert (not (successful? (parse simple-parser "b"))))

(assert (successful? (parse anychar "0")))
(assert (successful? (parse anychar "a")))
(assert (not (successful? (parse digit "a"))))
(assert (successful? (parse digit "0")))

(define open-close (sequence (char #\[) (char #\])))
(assert (successful? (parse open-close "[]")))
(assert (successful? (parse open-close "[])")))
(assert (not (successful? (parse open-close "[[]"))))
(assert (not (successful? (parse open-close "[t]"))))

(define alt-parens (either (char #\() (char #\))))
(assert (successful? (parse alt-parens "(")))
(assert (successful? (parse alt-parens ")")))
(assert (not (successful? (parse alt-parens "a("))))

(define parens
  (either
    (sequence (char #\()
              (sequence (lambda (x) (parens x))
                        (sequence (char #\))
                                  (lambda (x) (parens x)))))
    (succeed '())))


(assert (successful? (parse parens "()")))
(assert (successful? (parse parens "(((())))")))
;; succeed doesn't mark the end of the string, so this matches
(assert (successful? (parse parens "())"))) 
(assert (successful? (parse parens "(())()"))) 
(assert (not (successful? (parse parens "(()()"))))

(define aorb-bogus
  (either (sequence (char #\[) (char #\a))
          (sequence (char #\[) (char #\b))))
(assert (not (successful? (parse aorb-bogus "[b"))))
(assert (successful? (parse aorb-bogus "[a")))

(define aorb-right
  (either (try (sequence (char #\[) (char #\a)))
          (sequence (char #\[) (char #\b))))

(assert (successful? (parse aorb-right "[b")))
(assert (successful? (parse aorb-right "[a")))

(define parensbind
  (either
    (sequence (char #\()
              (bind (lambda (x) (parensbind x))
                    (lambda (n) 
                      (sequence 
                        (char #\))
                        (bind (lambda (x) (parensbind x))
                              (lambda (m) (succeed (max m (+ n 1)))))))))
    (succeed 0)))

(assert (= 3 (value (parse parensbind "((()))"))))
(assert (= 3 (value (parse parensbind "()((()))"))))
(assert (= 1 (value (parse parensbind "()"))))
(assert (not (successful? (parse parensbind "(()"))))

(assert (successful? (parse (many digit) "0912a")))
;; many accept even 0 occourrences
(assert (successful? (parse (many digit) "aaa")))

(assert (successful? (parse (many1 digit) "098a")))
(assert (equal? (string->list "098") (value (parse (many1 digit) "098a"))))
(assert (not (successful? (parse (many1 digit) "a90"))))
(assert (successful? (parse (many1 digit) "1aa2")))
(assert (equal? '(#\1) (value (parse (many1 digit) "1aa2"))))

