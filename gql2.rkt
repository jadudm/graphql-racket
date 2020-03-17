#lang racket
(require json struct-plus-plus)
(require (for-syntax syntax/parse))
(require "dbase.rkt")
(require db sql)

;; Everything is a hash table.
;; http://bit.ly/2TS92lR

;; Starting here.
;; https://graphql.org/learn/execution/

(struct _graphql-null ())
(define graphql-null (_graphql-null))

(define-syntax (define-type stx)
  (define-syntax-class field-defn
    #:description "field declaration"
    (pattern (field:id
              (~optional args #:defaults ([args #'()]))
              -> ret-type)))
  (syntax-parse stx
    [(_ _type (fields f:field-defn ...))
     (with-syntax ([field-hashes
                    #`(let ()
                        (define f* (quote (f.field ...)))
                        (define a* (quote (f.args ...)))
                        (define rt* (quote (f.ret-type ...)))
                        (for/list ([_f f*]
                                   [_a a*]
                                   [_rt rt*])
                          (define fh (make-hash))
                          (hash-set! fh 'field _f)
                          (hash-set! fh 'args _a)
                          (hash-set! fh 'field-type _rt)
                          fh))])
       #`(define _type
           (let ()
             (define h (make-hash))
             (define _fh field-hashes)
             (hash-set! h 'fields _fh)
             (hash-set! h 'type (quote _type))
             h))
       )])
  )

(define-syntax (define-enum stx)
  (syntax-parse stx
    [(_ type fields:id ...)
     #`(define type
         (let ([h (make-hash)])
           (define _f (quote (fields ...)))
           (for ([f _f]
                 [ndx (range (length _f))])
             (hash-set! h ndx f))
           h))]))

(define-type Starship
  (fields
   (name -> String)))

(define-enum Episode
  NEWHOPE EMPIRE JEDI)

(define-type Human
  (fields
   (name      -> String)
   (appearsIn -> (listof Episode))
   (starship  -> (listof Starship))
   ))        

(define-type Query
  (fields
   (human ([id ID!]) -> Human)
   ))

(define (->boolean o)
  (if o true false))

(define (is-type? typeh h)
  (->boolean
    (and
     (hash-has-key? h 'type)
     (hash-has-key? typeh 'type)
     (hash-has-key? typeh 'fields)
     (equal? (hash-ref typeh 'type)
             (hash-ref h 'type))
     (let* ([fields-in-h (hash-keys h)]
            [field-names-in-type
             (cons 'type 
                   (map (λ (h) (hash-ref h 'field)) 
                        (hash-ref typeh 'fields)))])
       (andmap (λ (k)
                 (member k field-names-in-type))
               fields-in-h)))))

(define NULL (void))
(define (ndx->enum enum ndx)
  (hash-ref enum ndx graphql-null))

(define (appearsIn o a c i)
  (define Q (select episode
                    #:from people_episode
                    #:where (= human_id ,(hash-ref o 'id))))
  ;; If you want to see the SQL generated...
  ;; (printf "Q: ~a~n" Q)
  (define result (query (hash-ref c 'conn) Q))
  ;; This gives me a set of rows from the people_episode table.
  ;; Then, I want to turn that into a list of enum elements.
  (for/list ([row (rows-result-rows result)])
    (ndx->enum Episode (vector-ref row 0))))
  

#|
(graphql-server
 (schema Query Human Episode Starship)
 (resolvers QueryR))
|#