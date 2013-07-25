(module json (run-json)
  (import r5rs chicken)
  (import parser state lexer basic-lexer)

  (defparser array
    (between (char #\[) (char #\]) 
             (named-bind
               (x <- (sep-by1 json (then (char #\,) trim)))
               trim
               (succeed x))))

  (defparser object
             (between (char #\{) (char #\}) 
                      (named-bind
                        trim
                        (x <- (sep-by1 pair (then (char #\,) trim)))
                        trim
                        (succeed x))))

  (define json 
    (either object string-literal number array))

  (defparser pair
    (named-bind 
      (x <- string-literal)
      (char #\:)
      trim
      (xs <- json)
      trim
      (succeed (cons x xs))))

  (define (run-json)
    (parse json data))

(define data "{\"firstName\": \"John\", 
  \"lastName\": \"Smith\", 
  \"age\": 25,
  \"address\":
  {
  \"streetAddress\": \"212ndStreet\",
  \"city\":\"NewYork\",
  
  \"state\":    \"NY\",
  \"postalCode\":\"10021\"
  },
  \"phoneNumber\": [{
                     \"type\":\"home\",\"number\":\"212555-1234\"},
                     {\"type\":\"fax\",\"number\":\"646555-4567\"}
                     ]
  
  }")
  )

