#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [prism? predicate/c]
  [make-prism
   (->* ((-> any/c option?) (-> any/c any/c))
        (#:name (or/c interned-symbol? #f))
        prism?)]
  [prism-match (-> prism? any/c option?)]
  [prism-cast (-> prism? any/c any/c)]
  [prism/c (-> contract? contract? contract?)]
  [identity-prism prism?]
  [present-prism (prism/c option? any/c)]
  [success-prism (prism/c result? any/c)]
  [failure-prism (prism/c result? any/c)]
  [predicate-prism
   (->i ([predicate predicate/c]) [_ (predicate) (prism/c any/c predicate)])]))

(require racket/bool
         racket/contract/combinator
         racket/match
         rebellion/base/converter
         rebellion/base/option
         rebellion/base/result
         rebellion/base/symbol
         rebellion/binary/bit
         rebellion/binary/byte
         rebellion/collection/entry
         rebellion/collection/immutable-vector
         rebellion/private/contract-projection
         rebellion/private/impersonation
         rebellion/private/static-name
         rebellion/type/object)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-object-type prism (matcher caster) #:constructor-name constructor:prism)

(define (make-prism matcher caster #:name [name #f])
  (constructor:prism
   #:matcher (fix-arity matcher)
   #:caster (fix-arity caster)
   #:name name))

(define (prism-match prism subject) ((prism-matcher prism) subject))
(define (prism-cast prism focus) ((prism-caster prism) focus))

(define (fix-arity prism-function)
  (if (equal? (procedure-arity prism-function) 1)
      prism-function
      (procedure-reduce-arity prism-function 1)))

;@------------------------------------------------------------------------------
;; Contracts

(define (prism-impersonate
         prism
         #:subject-input-guard [subject-input-guard #f]
         #:subject-output-guard [subject-output-guard #f]
         #:focus-input-guard [focus-input-guard #f]
         #:focus-output-guard [focus-output-guard #f]
         #:properties [properties (hash)]
         #:match-marks [match-marks (hash)]
         #:cast-marks [cast-marks (hash)]
         #:chaperone?
         [chaperone?
          (nor subject-input-guard
               subject-output-guard
               focus-input-guard
               focus-output-guard)])
  (define matcher (prism-matcher prism))
  (define caster (prism-caster prism))
  (define impersonated-matcher
    (function-impersonate
     matcher
     #:arguments-guard subject-input-guard
     #:results-guard (λ (opt) (option-map opt focus-output-guard))
     #:application-marks match-marks
     #:chaperone? chaperone?))
  (define impersonated-caster
    (function-impersonate
     caster
     #:arguments-guard focus-input-guard
     #:results-guard subject-output-guard
     #:application-marks cast-marks
     #:chaperone? chaperone?))
  (define impersonated-without-props
    (make-prism impersonated-matcher impersonated-caster
               #:name (object-name prism)))
  (object-impersonate impersonated-without-props descriptor:prism
                      #:properties properties))

(define/name (prism/c subject-contract* focus-contract*)
  (define subject-contract
    (coerce-contract enclosing-function-name subject-contract*))
  (define focus-contract
    (coerce-contract enclosing-function-name focus-contract*))
  (define contract-name
    (build-compound-type-name
     enclosing-function-name subject-contract focus-contract))
  (define subject-projection (contract-late-neg-projection subject-contract))
  (define focus-projection (contract-late-neg-projection focus-contract))
  (define chaperone?
    (and (chaperone-contract? subject-contract)
         (chaperone-contract? focus-contract)))
  (define (projection blame)
    (define subject-input-blame
      (blame-add-context blame "an input prism subject of"
                         #:swap? #t))
    (define subject-output-blame
      (blame-add-context blame "an output prism subject of"))
    (define focus-output-blame
      (blame-add-context blame "an output prism focus of"))
    (define focus-input-blame
      (blame-add-context blame "an input prism focus of" #:swap? #t))
    (define late-neg-subject-input-guard
      (subject-projection subject-input-blame))
    (define late-neg-focus-output-guard (focus-projection focus-output-blame))
    (define late-neg-focus-input-guard (focus-projection focus-input-blame))
    (define late-neg-subject-output-guard
      (subject-projection subject-output-blame))
    (λ (original-prism missing-party)
      (assert-satisfies original-prism prism? blame
                        #:missing-party missing-party)
      (define props
        (hash impersonator-prop:contracted the-contract
              impersonator-prop:blame (cons blame missing-party)))
      (define (subject-input-guard input)
        (late-neg-subject-input-guard input missing-party))
      (define (subject-output-guard output)
        (late-neg-subject-output-guard output missing-party))
      (define (focus-input-guard input)
        (late-neg-focus-input-guard input missing-party))
      (define (focus-output-guard output)
        (late-neg-focus-output-guard output missing-party))
      (prism-impersonate
       original-prism
       #:subject-input-guard subject-input-guard
       #:subject-output-guard subject-output-guard
       #:focus-input-guard focus-input-guard
       #:focus-output-guard focus-output-guard
       #:chaperone? chaperone?
       #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order prism?
     #:late-neg-projection projection))
  the-contract)

;@------------------------------------------------------------------------------
;; More prism constructors

(define (predicate-prism predicate)
  (make-prism (λ (v) (if (predicate v) (present v) absent)) values
              #:name (object-name predicate)))

(module+ test
  (test-case (name-string predicate-prism)
    (define only-strings (predicate-prism string?))
    (check-equal? (prism-match only-strings "foo") (present "foo"))
    (check-equal? (prism-match only-strings 420) absent)
    (check-equal? (prism-cast only-strings "foo") "foo")))

;@------------------------------------------------------------------------------
;; Standard library prisms

(define/name identity-prism
  (make-prism present values #:name enclosing-variable-name))

(module+ test
  (test-case (name-string identity-prism)
    (check-equal? (prism-match identity-prism 4) (present 4))
    (check-equal? (prism-cast identity-prism 'foo) 'foo)))


(define/name present-prism
  (make-prism values present #:name enclosing-variable-name))

(define (success-value res)
  (match res [(success v) (present v)] [(failure _) absent]))

(define/name success-prism
  (make-prism success-value success #:name enclosing-variable-name))

(define (failure-error res)
  (match res [(failure e) (present e)] [(success _) absent]))

(define/name failure-prism
  (make-prism failure-error failure #:name enclosing-variable-name))

(module+ test
  (test-case (name-string present-prism)
    (check-equal? (prism-match present-prism (present 5)) (present 5))
    (check-equal? (prism-match present-prism absent) absent)
    (check-equal? (prism-cast present-prism 5) (present 5)))

  (test-case (name-string success-prism)
    (check-equal? (prism-match success-prism (success 420)) (present 420))
    (check-equal? (prism-match success-prism (failure 'boom)) absent)
    (check-equal? (prism-cast success-prism 420) (success 420)))

  (test-case (name-string failure-prism)
    (check-equal? (prism-match failure-prism (failure 'boom)) (present 'boom))
    (check-equal? (prism-match failure-prism (success 420)) absent)
    (check-equal? (prism-cast failure-prism 'boom) (failure 'boom))))
