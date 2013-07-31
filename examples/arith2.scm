(module arith2 (run-arith)
  (import chicken r5rs)
  (import parser operator)

  ;;; this example is equivalent to the example in `arith.scm`, however, it's
  ;;; implemented from a higher level point of view, since we use combinators
  ;;; and we don't write the primitives ourselves. The other difference is 
  ;;; that here we interpret the operation during parsing.

  (defparser s
     (either
       (named-bind 
         (char #\() 
         (e <- expr)
         (char #\))
         (succeed e))
       number))

  ;; We adopt the (usual) convention that power is associative to the right.
  (define exponentiation (chainr1 s exp-op))
  (define factor (chainl1 exponentiation mul-op))
  (define expr (chainl1 factor add-op))
  
  (define (run-arith e)
    (parse expr e))
  )
