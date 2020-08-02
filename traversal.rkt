#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [traversal? predicate/c]
  [traversal/c (-> contract? contract? contract?)]
  [make-traversal
   (->* (#:getter (-> any/c list?)
         #:setter (-> any/c list? any/c)
         #:counter (-> any/c natural?))
        (#:name (or/c interned-symbol? #f))
        traversal?)]
  [traversal-count (-> traversal? any/c natural?)]
  [traversal-get-all (-> traversal? any/c list?)]
  [traversal-set-all (-> traversal? any/c (sequence/c any/c) any/c)]
  [list-traversal (traversal/c list? any/c)]
  [vector-traversal (traversal/c vector? any/c)]))

(require racket/bool
         racket/contract/combinator
         racket/math
         racket/sequence
         rebellion/base/symbol
         rebellion/collection/immutable-vector
         rebellion/collection/list
         rebellion/private/contract-projection
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/type/object)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-object-type traversal (getter setter counter)
  #:constructor-name constructor:traversal)

(define (make-traversal
         #:getter getter
         #:setter setter
         #:counter [counter (λ (s) (sequence-length (getter s)))]
         #:name [name #f])
  (constructor:traversal
   #:getter (function-reduce-arity getter 1)
   #:setter (function-reduce-arity setter 2)
   #:counter (function-reduce-arity counter 1)
   #:name name))

(define (function-reduce-arity function expected-arity)
  (if (equal? (procedure-arity function) expected-arity)
      function
      (procedure-reduce-arity function expected-arity)))

(define (traversal-count traversal subject)
  ((traversal-counter traversal) subject))

(define (traversal-get-all traversal subject)
  (sequence->list ((traversal-getter traversal) subject)))

(define/name (traversal-set-all traversal subject foci)
  (define original-count (traversal-count traversal subject))
  (define foci-list (sequence->list foci))
  (define replacement-count (list-size foci-list))
  (unless (equal? replacement-count original-count)
    (raise-arguments-error
     enclosing-function-name
     "traversals cannot be used to change the number of traversed elements"
     "traversal" traversal
     "original count" original-count
     "replacement count" replacement-count
     "subject" subject))
  ((traversal-setter traversal) subject foci-list))

(define/name list-traversal
  (make-traversal
   #:getter values
   #:setter (λ (_ replacements) replacements)
   #:counter list-size
   #:name enclosing-variable-name))

(define/name vector-traversal
  (make-traversal
   #:getter sequence->list
   #:setter (λ (_ replacements) (list->immutable-vector replacements))
   #:counter vector-length
   #:name enclosing-variable-name))

(module+ test
  (test-case (name-string traversal-set-all)
    (define (set-too-many)
      (traversal-set-all list-traversal (list 1 2 3) (list 4 5 6 7 8)))
    (check-exn exn:fail:contract? set-too-many)
    (check-exn #rx"traversal-set-all" set-too-many)
    (check-exn #rx"original count: 3" set-too-many)
    (check-exn #rx"replacement count: 5" set-too-many))
  
  (test-case (name-string list-traversal)
    (check-equal? (traversal-count list-traversal (list 1 2 3)) 3)
    (check-equal? (traversal-get-all list-traversal (list 1 2 3)) (list 1 2 3))
    (check-equal?
     (traversal-set-all list-traversal (list 1 2 3) (list 4 5 6)) (list 4 5 6)))

  (test-case (name-string vector-traversal)
    (check-equal? (traversal-count vector-traversal (vector 1 2 3)) 3)
    (check-equal? (traversal-count vector-traversal (immutable-vector 1 2 3)) 3)
    (check-equal?
     (traversal-get-all vector-traversal (vector 1 2 3)) (list 1 2 3))
    (check-equal?
     (traversal-get-all vector-traversal (immutable-vector 1 2 3)) (list 1 2 3))
    (define vec (vector 1 2 3))
    (define result-vec (traversal-set-all vector-traversal vec (list 4 5 6)))
    (check-equal? vec (vector 1 2 3))
    (check-not-equal? result-vec vec)
    (check-pred immutable? result-vec)
    (check-equal? result-vec (immutable-vector 4 5 6))))

;@------------------------------------------------------------------------------
;; Contracts

(define (traversal-impersonate
         traversal
         #:subject-input-guard [subject-input-guard #f]
         #:subject-output-guard [subject-output-guard #f]
         #:foci-input-guard [foci-input-guard #f]
         #:foci-output-guard [foci-output-guard #f]
         #:properties [properties (hash)]
         #:count-marks [count-marks (hash)]
         #:get-marks [get-marks (hash)]
         #:set-marks [set-marks (hash)]
         #:chaperone?
         [chaperone?
          (nor subject-input-guard
               subject-output-guard
               foci-input-guard
               foci-output-guard)])
  (define counter (traversal-counter traversal))
  (define getter (traversal-getter traversal))
  (define setter (traversal-setter traversal))
  (define chaperoned-counter
    (function-impersonate counter #:application-marks count-marks))
  (define impersonated-getter
    (function-impersonate
     getter
     #:arguments-guard subject-input-guard
     #:results-guard (λ (foci) (map foci-output-guard foci))
     #:application-marks get-marks
     #:chaperone? chaperone?))
  (define impersonated-setter
    (function-impersonate
     setter
     #:arguments-guard
     (λ (subject replacement-foci)
       (values (subject-input-guard subject)
               (map foci-input-guard replacement-foci)))
     #:results-guard subject-output-guard
     #:application-marks set-marks
     #:chaperone? chaperone?))
  (define impersonated-without-props
    (make-traversal
     #:getter impersonated-getter
     #:setter impersonated-setter
     #:counter chaperoned-counter
     #:name (object-name traversal)))
  (object-impersonate
   impersonated-without-props descriptor:traversal
   #:properties properties))

(define/name (traversal/c subject-contract* foci-contract*)
  (define subject-contract
    (coerce-contract enclosing-function-name subject-contract*))
  (define foci-contract
    (coerce-contract enclosing-function-name foci-contract*))
  (define contract-name
    (build-compound-type-name
     enclosing-function-name subject-contract foci-contract))
  (define subject-projection (contract-late-neg-projection subject-contract))
  (define foci-projection (contract-late-neg-projection foci-contract))
  (define chaperone?
    (and (chaperone-contract? subject-contract)
         (chaperone-contract? foci-contract)))
  (define (projection blame)
    (define subject-input-blame
      (blame-add-context
       blame "an input traversal subject of" #:swap? #t))
    (define subject-output-blame
      (blame-add-context blame "an output traversal subject of"))
    (define foci-output-blame
      (blame-add-context blame "an output traversal focus of"))
    (define foci-input-blame
      (blame-add-context blame "an input traversal focus of" #:swap? #t))
    (define late-neg-subject-input-guard
      (subject-projection subject-input-blame))
    (define late-neg-foci-output-guard (foci-projection foci-output-blame))
    (define late-neg-foci-input-guard (foci-projection foci-input-blame))
    (define late-neg-subject-output-guard
      (subject-projection subject-output-blame))
    (λ (original-traversal missing-party)
      (assert-satisfies
       original-traversal traversal? blame #:missing-party missing-party)
      (define props
        (hash impersonator-prop:contracted the-contract
              impersonator-prop:blame (cons blame missing-party)))
      (define (subject-input-guard input)
        (late-neg-subject-input-guard input missing-party))
      (define (subject-output-guard output)
        (late-neg-subject-output-guard output missing-party))
      (define (foci-input-guard input)
        (late-neg-foci-input-guard input missing-party))
      (define (foci-output-guard output)
        (late-neg-foci-output-guard output missing-party))
      (traversal-impersonate
       original-traversal
       #:subject-input-guard subject-input-guard
       #:subject-output-guard subject-output-guard
       #:foci-input-guard foci-input-guard
       #:foci-output-guard foci-output-guard
       #:chaperone? chaperone?
       #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order traversal?
     #:late-neg-projection projection))
  the-contract)
