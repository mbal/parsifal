(module arith2 (run-arith)
  (import chicken r5rs)
  (import parser operator)

  ;;; this example is equivalent to the example in `arith.scm`, however, it's
  ;;; implemented from a higher level point of view, since we use combinators
  ;;; and we don't write the primitives ourselves.

  (defparser s
     (either
       (named-bind 
         (char #\() 
         (e <- expr2)
         (char #\))
         (succeed e))
       number))

  (define f (chainl1 s2 mul-op))
  (define expr (chainl1 f2 add-op))
  
  (define (run-arith e)
    (parse expr e))
  )
