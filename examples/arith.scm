(module arith ()
  (import chicken r5rs)
  (import parser state utils)

  ;;; EXPR = FACT + EXPR | FACT - EXPR | FACT
  ;;; FACT = S * FACT | S / FACT | S
  ;;; S = ( EXPR ) | NUMBER
  ;;;
  (defparser s
     (either
       (named-bind 
         (char #\() 
         (e <- expr)
         (char #\))
         (succeed e))
       number))

  (define fact 
    (either
      (try (named-bind
        (f1 <- s)
        (char #\*)
        (f2 <- fact)
        (succeed (list '* f1 f2))))
      s))

  (define expr
    (either
      (try 
        (named-bind
        (f1 <- fact)
        (char #\+)
        (f2 <- expr)
        (succeed (list '+ f1 f2))))
      fact))
)
