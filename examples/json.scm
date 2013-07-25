(module json (run-json)
  (import r5rs chicken)
  (import parser basic-lexer)

  (defparser array
    (inside-bracket (char #\[) (char #\]) (comma-sep json)))

  (defparser object
             (inside-bracket (char #\{) (char #\}) (comma-sep pair)))

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
  \"phoneNumber\": [
                    {
                     \"type\":\"home\",\"number\":\"212555-1234\"},
                     {\"type\":\"fax\",\"number\":\"646555-4567\"}
                     ]
  
  }")
  )

