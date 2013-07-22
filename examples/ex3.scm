(module ex3 ()
  (import r5rs chicken parser)
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
  ;; fix the problem

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
  )
