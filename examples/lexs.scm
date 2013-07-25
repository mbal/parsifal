(module lexs (run-json data json)
  (import r5rs chicken)
  (import parser state lexer)

  (define escape '(#\" #\\))
  (define escape-error "invalid escape sequence")

  (defparser char-literal
             (either
               (<?> (then (char #\\) escape) escape-error)
               (satisfy (lambda (x) (and (not (char=? x #\"))
                                         (not (char=? x #\\)))))))

  (defparser string-literal
             (stringify (between (char #\") (char #\") char-literal)))

  (define pair
    (bind string-literal
          (lambda (x) (then 
                        (char #\:)
                        (bind json 
                              (lambda (xs) 
                                (succeed (cons x xs))))))))

  (define array
    (between (char #\[) (char #\]) (sep-by1 json (char #\,))))

  (define object
    (between
      (char #\{) (char #\})
      (bind (sep-by1 pair (char #\,))
            (lambda (x) (succeed x)))))

  (define json
    (either object string-literal number array object))

  (define (run-json)
    (parse json data))


(define data "{\"firstName\":\"John\",\"lastName\":\"Smith\",\"age\":25,\"address\":{\"streetAddress\":\"212ndStreet\",\"city\":\"NewYork\",\"state\":\"NY\",\"postalCode\":\"10021\"},\"phoneNumber\":[{\"type\":\"home\",\"number\":\"212555-1234\"},{\"type\":\"fax\",\"number\":\"646555-4567\"}]}")
  )

