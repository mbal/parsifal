;;; miscellaneous parsers for arithmetic.

(module arith (chainl chainl1 chainr1 expr)
  (import chicken r5rs)
  (import parser state utils)

  ;; parses p, and, as long as there is a binary operation op, reads it and
  ;; another p, then applies `perform` on the two values. The operator
  ;; should associate to the left
  (define (chainl1 p op perform)
    (begin
      (define (func prev) 
        (either (named-bind op (b <- p) (func (perform prev b)))
                (succeed prev)))
      (named-bind
        (a <- p)
        (in <- (func a))
        (succeed in))))

  ;; as chainl1, but returns a default if there's no initial match
  (define (chainl p op perform default)
    (either (chainl1 p op perform) (succeed default)))

  ;;; accumulates p to the right. While it parses the sequence op p, it 
  ;;; traverse the string. When there are no more op, it rewinds itself,
  ;;; folding back the results of p, applying `perform`.
  (define (chainr1 p op perform)
    (named-bind
      (a <- p)
      (either
        (named-bind
          op
          (b <- (chainr1 p op perform))
          (succeed (perform a b)))
        (succeed a))))

  (define (chainr p op perform default)
    (either (chainr1 p op perform) (succeed default)))


  ;;; EXPR = FACT + EXPR | FACT
  ;;; FACT = FACT * S | S
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
      (try (named-bind
        (f1 <- fact)
        (char #\+)
        (f2 <- expr)
        (succeed (list '+ f1 f2))))
      fact))

)
