(module arith (add-op)
  (import chicken r5rs)
  (import parser state utils)

;  (define (add-op p q)
;    (named-bind
;      (skip-many (char #\space))
;      (a <- p)
;      (skip-many (char #\space))
;      (char #\+)
;      (skip-many (char #\space))
;      (b <- p)
;      (skip-many (char #\space))
;      (succeed (+ a b))))
;
  ;;; EXPR = FACT + EXPR | FACT
  ;;; FACT = S * FACT | S
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
