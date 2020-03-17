#lang racket

(struct query (pattern)
  #:transparent)
(struct resolver (pattern action)
  #:transparent)

(define (parse-query q)
  (match q
    [(list 'query pat) (query (first pat))]
    [else
     (error 'parse-query
            "Invalid query form: ~s~n" q)]))

(define-syntax-rule (build-schema q ...)
  (let ()
    (define quoted (list (quasiquote q) ...))
    (define parsed (map parse-query quoted))
    parsed))

(define (parse-resolver r)
  (match r
    [(list 'resolver query fun)
     (resolver query fun)]))

(define-syntax-rule (build-resolvers r ...)
  (let ()
    (define quoted (map (λ (o)
                          (resolver
                           (second o)
                           (eval (third o)))) (list (quasiquote r ...))))
      (define parsed (map parse-resolver quoted))
    parsed))
     
  
(define (hello-fun)
  "hello!")
  
(define schema
  (build-schema
   (query (hello))
   (query (hello (name)))
   ))

(define (root q)
  (match q
    [(query 'hello) hello-fun]))

