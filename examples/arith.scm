(module arith (run-arith)
  (import chicken r5rs)
  (import parser state utils)

  ;;; We will build a parse tree from a arithmetic expression over the
  ;;; integers with the 4 operations + - * /.
  ;;; After the parse tree, the evaluation is almost natural, in a LISP
  ;;; (basically, just call eval), and in other languages is quite easy
  ;;; as well, since a primitive version of eval on a tree is trivial.
  ;;; The interpretation during parsing is very easy as well.
  ;;;
  ;;; Once we have defined the grammar, the rest is almost trivial.
  ;;; The grammar must be defined this way, otherwise the precedences
  ;;; won't work. 
  ;;; EXPR = FACT + EXPR | FACT - EXPR | FACT
  ;;; FACT = S * FACT | S / FACT | S
  ;;; S = ( EXPR ) | NUMBER
  ;;;
  ;;; This grammar is translated "as-is" in the source code, which is a 
  ;;; great advantage of the parser combinators.
  ;;; Note however, that this formulation doesn't allow spaces between
  ;;; operators and numbers (basically, 5*3 is valid, while 5 * 3 is not not).
  ;;; To address this issue we should add (skip-many (char #\space)) after
  ;;; every line in the parser, which pollutes the code and makes it
  ;;; difficult to follow.
  ;;;
  ;;; The only tricky part in the grammar is the try in FACT and EXPR. 
  ;;; This is the first time we used it, and it's necessary in order
  ;;; to continue to match all the other possibilities after
  ;;; the first fails.
  ;;; Basically, we need look-ahead in order to know if we are matching
  ;;; an operation or a standalone term.
  ;;; If we don't use the try, we end up in a situation like this 
  ;;; (during the match of a e.g. division)
  ;;;
  ;;; (1 + 2) + 4 ==> we match (1 + 2) against FACT (first rule in EXPR)
  ;;; (1 + 2) triggers the matching of the first rule in FACT, S * FACT, 
  ;;; so, we match S against (1 + 2), which succeed, and then we look for 
  ;;; a * sign, which fails. According to the definition of either, when
  ;;; has consumed input and it fails, the whole either fails. So, in this
  ;;; case, FACT matching fails and EXPR fails too, even though the last
  ;;; rule in FACT matched.
  ;;;
  ;;; If we use try, however, when we arrive at *, we fail, and return to the
  ;;; parser a new *failing* state, but saying that the parser hasn't 
  ;;; consumed any input; blatant lie, yes, but it's in fact what we needed.
  ;;; Another solution would be LEFT-FACTOR the grammar, which I think it's 
  ;;; always possbile, but I have yet to come up with a proof.
  ;;; However, left-factor is often difficult, and the try primitive 
  ;;; makes our job easier.
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
        (op <- (either (then (char #\*) (succeed '*))
                       (then (char #\/) (succeed '/))))
        (f2 <- fact)
        (succeed (list op f1 f2))))
      s))

  (define expr
    (either
      (try 
        (named-bind
        (f1 <- fact)
        (op <- (either (then (char #\+) (succeed '+))
                       (then (char #\-) (succeed '-))))
        (f2 <- expr)
        (succeed (list op f1 f2))))
      fact))

  (define (run-arith e)
    (parse expr e))
)
