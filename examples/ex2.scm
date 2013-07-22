(module ex2 ()
  (import parser chicken r5rs)

  (define (displayln x) (display x) (newline))

  ;; count the nesting level of parens
  (displayln "count the nesting level of parens")

  (define nesting
    (either
      (named-bind
        (char #\()
        (n <- (lambda (x) (nesting x)))
        (char #\))
        (m <- (lambda (x) (nesting x)))
        (succeed (max m (+ 1 n))))
      (succeed 0)))

  (displayln (run nesting "((()))"))  ;;; ==> ok, 3
  (displayln (run nesting "()()()"))  ;;; ==> ok, 1
  (displayln (run nesting "(()"))     ;;; ==> unexpected EOF
  (displayln (run nesting "(r)"))     ;;; ==> unexpected r

  ;; split the words at the tokens
  ;; PUNCT -> , | . | space | ! | ;
  ;; WORD -> CHARACTER*
  ;; WORDS -> (WORD PUNCT*)*
  (displayln "split the words at the tokens")
  (define punct (one-of '(#\, #\space #\. #\! #\;))) ;; equivalent to either
  (define words (sep-by1 word (skip-many1 punct)))

  (displayln (parse words "words,. Words;words")) 
  ;;; ==> ("words" "Words" "words")
)
