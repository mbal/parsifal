(module ex3 ()
  (import r5rs chicken parser)
  (define (displayln x) (display x) (newline))
  ;; a decimal digit recognizer.
  ;;
  ;; NUMBER -> SIGN DIGIT NUMBER | empty
  ;; SIGN -> + | - | empty
  ;; The recognizer itself is quite easy

  (define sign
    (either (char #\+) (char #\-)))

  (define numb
    (many1 digit))

  (define decnumber 
    (either
      (named-bind 
        (s <- sign)
        (n <- (<?> numb "expected number"))
        (succeed (cons s n)))
      numb))

  ;; this parser will work (almost) as expected
  ;; (run decnumber "42") ==> (#\4 #\2)
  ;; (run decnumber "-22") ==> (#\- #\2 #\2)
  ;; (run decnumber "-forty-two") ==> expected number
  ;; (run decnumber "forty-two") ==> unexpected f
  ;;
  ;; This is almost right. We don't want a list of digits, but a real number,
  ;; as result. Succeed can contain any scheme code, however, so we can easily
  ;; fix the problem. The complete conversion can also be done during matching
  ;; time, however, it's a bit more verbose and less intuitive.
  
  (define decnumber 
    (either
      (named-bind 
        (s <- sign)
        (n <- (<?> numb "expected number"))
        (succeed (func s n)))
      (named-bind
        (n <- numb)
        (succeed (func #\+ n)))))

  (define (func sign num)
    (let ((n (string->number (list->string num))))
      (if (char=? sign #\+)
        n
        (- n))))

  (displayln (run decnumber "+21")) ;; ==> 21
  (displayln (run decnumber "-21")) ;; ==> -21
  (displayln (run decnumber "42")) ;; ==> 42
  (displayln (run decnumber "a42")) ;; ==> unexpected a
  (displayln (run decnumber "+a42")) ;; ==> expected number
  
  ;; Fortunately, there is already a parser for natural number defined in 
  ;; parsifal.
  ;; Using that one, the recognizer becomes:
  (defparser dec-number
             (either
               (named-bind
                 (s <- sign)
                 (n <- number)
                 (succeed (if (char=? #\+ s) n (- n))))
               number))

  (displayln (run dec-number "+21")) ;; ==> 21
  (displayln (run dec-number "-21")) ;; ==> -21
  (displayln (run dec-number "42")) ;; ==> 42
  (displayln (run dec-number "a42")) ;; ==> expected number
  (displayln (run dec-number "+a42")) ;; ==> expected number

  )
