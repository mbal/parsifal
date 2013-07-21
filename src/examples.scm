;; NOTE : I am still working on the syntax

(define (displayln x) (display x) (newline))

;; to match couple of parenthesis we use the following grammar:
;; S -> [S] | empty
(define single-parens
  (either
    (then (char #\[)
          (then (lambda (x) (single-parens x))
                (char #\])))
    (succeed '())))

;; all successful
(displayln (parse single-parens "[]"))
(displayln (parse single-parens "[[]]"))
;; successful: the parsing ends at the second ] we encounter, and we stop:
;; the parser doesn't say the string should end when the parser finish.
(displayln (parse single-parens "[[]][]"))
;; successful: a bit more surprising, but the reason is the same as above
(displayln (parse single-parens "[]][]"))
;; not successful: unexpected [ at index 3.
(displayln (parse single-parens "[[][]"))

;; expansion of the previous example, we match couple of parenthesis,
;; but we allow arbitrary nesting (however, it should still be valid).
;; For example: ()()()() this is allowed
;; while ()) is not.
;;
;; S -> (S)S | empty
(define parens
  (either
    (then (char #\()
          (then (lambda (x) (parens x))
                (then (char #\))
                      (lambda (x) (parens x)))))
    (succeed '())))

(displayln (parse parens "((()))"))  ;;; ==> ok
(displayln (parse parens "()()()"))  ;;; ==> ok
(displayln (parse parens "(()"))     ;;; ==> unexpected EOF
(displayln (parse parens "(r)"))     ;;; ==> unexpected r

;; split the words at the tokens
;; PUNCT -> , | . | space | ! | ;
;; WORD -> CHARACTER*
;; WORDS -> (WORD PUNCT*)*
(define punct (oneOf '(#\, #\space #\. #\! #\;))) ;; equivalent to either
(define words (sepBy1 word (skipMany1 punct)))

(displayln (parse words "words,. Words;words")) 
;;; ==> ("words" "Words" "words")
