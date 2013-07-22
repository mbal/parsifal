(module state (successful? make-state copy-state-except
               input position value empty? error)

  (import r5rs chicken)

  (define (successful? state) (equal? (error state) #f))

  (define (make-state input pos value empty? err)
    (list input pos value empty? err))

  (define (copy-state-except original field new-value)
    (apply make-state
           (map (lambda (x) 
                  (if (not (equal? field x)) 
                    (x original) 
                    new-value))
                (list input position value empty? error))))

  (define input car)
  (define position cadr)
  (define value caddr)
  (define empty? cadddr)
  (define (error x) (car (cddddr x))))
