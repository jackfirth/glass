#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [indexed-traversal? predicate/c]
  [make-indexed-traversal
   (->* (#:getter (-> any/c immutable-vector?)
         #:indexer (-> any/c immutable-vector?)
         #:setter (-> any/c immutable-vector? any/c)
         #:counter (-> any/c natural?))
        (#:name (or/c interned-symbol? #f))
        indexed-traversal?)]
  [indexed-traversal-get-all (-> indexed-traversal? any/c immutable-vector?)]
  [indexed-traversal-set-all
   (-> indexed-traversal? any/c (sequence/c any/c) any/c)]
  [indexed-traversal-get-all-indices
   (-> indexed-traversal? any/c immutable-vector?)]
  [indexed-traversal-map
   (-> indexed-traversal? any/c (-> any/c any/c any/c) any/c)]))

(require racket/math
         racket/sequence
         rebellion/base/symbol
         rebellion/collection/immutable-vector
         rebellion/collection/vector
         rebellion/private/static-name
         rebellion/type/object)

(module+ test
  (require (submod "..")
           fancy-app
           rackunit))

;@------------------------------------------------------------------------------

(define-object-type indexed-traversal
  (indexer getter setter counter)
  #:constructor-name constructor:indexed-traversal)

(define (make-indexed-traversal
         #:getter getter
         #:indexer indexer
         #:setter setter
         #:counter [counter (λ (s) (immutable-vector-length (getter s)))]
         #:name [name #f])
  (constructor:indexed-traversal
   #:getter (function-reduce-arity getter 1)
   #:indexer (function-reduce-arity indexer 1)
   #:setter (function-reduce-arity setter 2)
   #:counter (function-reduce-arity counter 1)
   #:name name))

(define (function-reduce-arity function expected-arity)
  (if (equal? (procedure-arity function) expected-arity)
      function
      (procedure-reduce-arity function expected-arity)))

(define (indexed-traversal-get-all-indices traversal subject)
  ((indexed-traversal-indexer traversal) subject))

(define (indexed-traversal-get-all traversal subject)
  ((indexed-traversal-getter traversal) subject))

(define (indexed-traversal-count traversal subject)
  ((indexed-traversal-counter traversal) subject))

(define/name (indexed-traversal-set-all traversal subject foci)
  (define original-count (indexed-traversal-count traversal subject))
  (define foci-vec (sequence->vector foci))
  (define replacement-count (immutable-vector-length foci-vec))
  (unless (equal? replacement-count original-count)
    (raise-arguments-error
     enclosing-function-name
     "traversals cannot be used to change the number of traversed elements"
     "traversal" traversal
     "original count" original-count
     "replacement count" replacement-count
     "subject" subject))
  ((indexed-traversal-setter traversal) subject foci-vec))

(define (indexed-traversal-map traversal subject indexed-mapper)
  (define indices (indexed-traversal-get-all-indices traversal subject))
  (define foci (indexed-traversal-get-all traversal subject))
  (define count (immutable-vector-length indices))
  (define replacements
    (for/vector #:length count
      ([i (in-vector indices)]
       [v (in-vector foci)])
      (indexed-mapper i v)))
  (indexed-traversal-set-all traversal subject replacements))

(define/name hash-traversal
  (make-indexed-traversal
   #:getter
   (λ (h) (vector->immutable-vector (for/vector ([(_ v) (in-hash h)]) v)))
   #:indexer
   (λ (h) (vector->immutable-vector (for/vector ([(k _) (in-hash h)]) k)))
   #:setter
   (λ (h replacements)
     (for/hash ([(k _) (in-hash h)] [v (in-vector replacements)]) (values k v)))
   #:counter hash-count
   #:name enclosing-variable-name))

(module+ test
  (test-case (name-string hash-traversal)
    (define h (hash 'a 1 'b 2 'c 3))
    (define show-entry (format "~a:~a" _ _))
    (check-equal?
     (indexed-traversal-map hash-traversal h show-entry)
     (hash 'a "a:1" 'b "b:2" 'c "c:3"))))
