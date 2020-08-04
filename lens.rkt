#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [lens? predicate/c]
  [make-lens
   (->* ((-> any/c any/c) (-> any/c any/c any/c))
        (#:name (or/c interned-symbol? #f))
        lens?)]
  [lens-get (-> lens? any/c any/c)]
  [lens-set (-> lens? any/c any/c any/c)]
  [lens/c (-> contract? contract? contract?)]
  [lens-pipe (-> lens? ... lens?)]
  [pair.first (lens/c pair? any/c)]
  [pair.second (lens/c pair? any/c)]
  [entry.key (lens/c entry? any/c)]
  [entry.value (lens/c entry? any/c)]
  [byte.bit (-> (integer-in 0 7) (lens/c byte? bit?))]
  [identity-lens lens?]
  [forward-converter-lens (-> converter? lens?)]
  [backward-converter-lens (-> converter? lens?)]))

(require racket/bool
         racket/contract/combinator
         racket/match
         rebellion/base/converter
         rebellion/base/pair
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

(define-object-type lens (getter setter) #:constructor-name constructor:lens)

(define (make-lens getter setter #:name [name #f])
  (constructor:lens
   #:getter (fix-getter-arity getter)
   #:setter (fix-setter-arity setter)
   #:name name))

(define (fix-getter-arity getter)
  (if (equal? (procedure-arity getter) 1)
      getter
      (procedure-reduce-arity getter 1)))

(define (fix-setter-arity setter)
  (if (equal? (procedure-arity setter) 2)
      setter
      (procedure-reduce-arity setter 2)))

(define (lens-get lens subject) ((lens-getter lens) subject))

(define (lens-set lens subject replacement-focus)
  ((lens-setter lens) subject replacement-focus))

;@------------------------------------------------------------------------------
;; Contracts

(define (lens-impersonate
         lens
         #:subject-input-guard [subject-input-guard #f]
         #:subject-output-guard [subject-output-guard #f]
         #:focus-input-guard [focus-input-guard #f]
         #:focus-output-guard [focus-output-guard #f]
         #:properties [properties (hash)]
         #:get-marks [get-marks (hash)]
         #:set-marks [set-marks (hash)]
         #:chaperone?
         [chaperone?
          (nor subject-input-guard
               subject-output-guard
               focus-input-guard
               focus-output-guard)])
  (define getter (lens-getter lens))
  (define setter (lens-setter lens))
  (define impersonated-getter
    (function-impersonate
     getter
     #:arguments-guard subject-input-guard
     #:results-guard focus-output-guard
     #:application-marks get-marks
     #:chaperone? chaperone?))
  (define impersonated-setter
    (function-impersonate
     setter
     #:arguments-guard
     (λ (subject replacement-focus)
       (values (subject-input-guard subject)
               (focus-input-guard replacement-focus)))
     #:results-guard subject-output-guard
     #:application-marks set-marks
     #:chaperone? chaperone?))
  (define impersonated-without-props
    (make-lens impersonated-getter impersonated-setter
               #:name (object-name lens)))
  (object-impersonate impersonated-without-props descriptor:lens
                      #:properties properties))

(define/name (lens/c subject-contract* focus-contract*)
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
      (blame-add-context blame "an input lens subject of"
                         #:swap? #t))
    (define subject-output-blame
      (blame-add-context blame "an output lens subject of"))
    (define focus-output-blame
      (blame-add-context blame "an output lens focus of"))
    (define focus-input-blame
      (blame-add-context blame "an input lens focus of" #:swap? #t))
    (define late-neg-subject-input-guard
      (subject-projection subject-input-blame))
    (define late-neg-focus-output-guard (focus-projection focus-output-blame))
    (define late-neg-focus-input-guard (focus-projection focus-input-blame))
    (define late-neg-subject-output-guard
      (subject-projection subject-output-blame))
    (λ (original-lens missing-party)
      (assert-satisfies original-lens lens? blame
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
      (lens-impersonate
       original-lens
       #:subject-input-guard subject-input-guard
       #:subject-output-guard subject-output-guard
       #:focus-input-guard focus-input-guard
       #:focus-output-guard focus-output-guard
       #:chaperone? chaperone?
       #:properties props)))
  (define the-contract
    ((if chaperone? make-chaperone-contract make-contract)
     #:name contract-name
     #:first-order lens?
     #:late-neg-projection projection))
  the-contract)

;@------------------------------------------------------------------------------
;; More lens constructors

(define/name (forward-converter-lens converter)
  (make-lens (λ (subject) (convert-forward converter subject))
             (λ (_ replacement-focus)
               (convert-backward converter replacement-focus))
             #:name enclosing-function-name))

(define/name (backward-converter-lens converter)
  (make-lens (λ (subject) (convert-backward converter subject))
             (λ (_ replacement-focus)
               (convert-forward converter replacement-focus))
             #:name enclosing-function-name))

(module+ test
  (test-case (name-string forward-converter-lens)
    (define string.symbol (forward-converter-lens string<->symbol))
    (check-equal? (lens-get string.symbol "foo") 'foo)
    (check-equal? (lens-set string.symbol "unused" 'foo) "foo"))

  (test-case (name-string backward-converter-lens)
    (define symbol.string (backward-converter-lens string<->symbol))
    (check-equal? (lens-get symbol.string 'bar) "bar")
    (check-equal? (lens-set symbol.string 'unused "bar") 'bar)))

(define (lens-pipe-getter outer-lens inner-lens)
  (define outer-getter (lens-getter outer-lens))
  (define inner-getter (lens-getter inner-lens))
  (λ (subject) (inner-getter (outer-getter subject))))

(define (lens-pipe-setter outer-lens inner-lens)
  (define outer-getter (lens-getter outer-lens))
  (define outer-setter (lens-setter outer-lens))
  (define inner-setter (lens-setter inner-lens))
  (λ (subject replacement)
    (define outer-replacement (inner-setter (outer-getter subject) replacement))
    (outer-setter subject outer-replacement)))

(define (lens-pipe2 outer-lens inner-lens)
  (define getter (lens-pipe-getter outer-lens inner-lens))
  (define setter (lens-pipe-setter outer-lens inner-lens))
  (make-lens getter setter #:name 'piped))

(define (lens-pipe . lenses)
  (match lenses
    ['() identity-lens]
    [(list lens) lens]
    [(cons first-lens remaining-lenses)
     (for/fold ([piped first-lens]) ([lens remaining-lenses])
       (lens-pipe2 piped lens))]))

(module+ test
  (test-case (name-string lens-pipe)

    (test-case "identity lens"
      (check-equal? (lens-pipe) identity-lens))

    (test-case "single lens"
      (check-equal? (lens-pipe entry.key) entry.key))

    (test-case "two lenses"
      (define entry.value.first-bit (lens-pipe entry.value (byte.bit 0)))
      (define data (entry 'a (byte 0 0 0 0 0 0 0 0)))
      (define expected (entry 'a (byte 1 0 0 0 0 0 0 0)))
      (check-equal? (lens-get entry.value.first-bit data) 0)
      (check-equal? (lens-set entry.value.first-bit data 1) expected))

    (test-case "many lenses"
      (define entry.value.key.first-bit
        (lens-pipe entry.value entry.key (byte.bit 0)))
      (define data (entry 'a (entry (byte 0 0 0 0 0 0 0 0) 'b)))
      (define expected (entry 'a (entry (byte 1 0 0 0 0 0 0 0) 'b)))
      (check-equal? (lens-get entry.value.key.first-bit data) 0)
      (check-equal? (lens-set entry.value.key.first-bit data 1) expected))))

;@------------------------------------------------------------------------------
;; Standard library lenses

(define/name identity-lens
  (make-lens values (λ (_ v) v) #:name enclosing-variable-name))

(module+ test
  (test-case (name-string identity-lens)
    (check-equal? (lens-get identity-lens 4) 4)
    (check-equal? (lens-set identity-lens 'unused 3) 3)))

(define/name pair.first
  (make-lens pair-first (λ (p v) (pair v (pair-second p)))
             #:name enclosing-variable-name))

(define/name pair.second
  (make-lens pair-second (λ (p v) (pair (pair-first p) v))
             #:name enclosing-variable-name))

(define/name entry.key
  (make-lens entry-key (λ (e k) (entry k (entry-value e)))
             #:name enclosing-variable-name))

(define/name entry.value
  (make-lens entry-value (λ (e v) (entry (entry-key e) v))
             #:name enclosing-variable-name))

(module+ test
  (test-case (name-string pair.first)
    (check-equal? (lens-get pair.first (pair 'a 1)) 'a)
    (check-equal? (lens-set pair.first (pair 'a 1) 'b) (pair 'b 1)))

  (test-case (name-string pair.second)
    (check-equal? (lens-get pair.second (pair 'a 1)) 1)
    (check-equal? (lens-set pair.second (pair 'a 1) 2) (pair 'a 2)))

  (test-case (name-string entry.key)
    (check-equal? (lens-get entry.key (entry 'a 1)) 'a)
    (check-equal? (lens-set entry.key (entry 'a 1) 'b) (entry 'b 1)))

  (test-case (name-string entry.value)
    (check-equal? (lens-get entry.value (entry 'a 1)) 1)
    (check-equal? (lens-set entry.value (entry 'a 1) 2) (entry 'a 2))))


(define (byte-set-first b bit)
  (if (zero? bit)
      (byte-and b (byte 0 1 1 1 1 1 1 1))
      (byte-or b (byte 1 0 0 0 0 0 0 0))))

(define (byte-set-second b bit)
  (if (zero? bit)
      (byte-and b (byte 1 0 1 1 1 1 1 1))
      (byte-or b (byte 0 1 0 0 0 0 0 0))))

(define (byte-set-third b bit)
  (if (zero? bit)
      (byte-and b (byte 1 1 0 1 1 1 1 1))
      (byte-or b (byte 0 0 1 0 0 0 0 0))))

(define (byte-set-fourth b bit)
  (if (zero? bit)
      (byte-and b (byte 1 1 1 0 1 1 1 1))
      (byte-or b (byte 0 0 0 1 0 0 0 0))))

(define (byte-set-fifth b bit)
  (if (zero? bit)
      (byte-and b (byte 1 1 1 1 0 1 1 1))
      (byte-or b (byte 0 0 0 0 1 0 0 0))))

(define (byte-set-sixth b bit)
  (if (zero? bit)
      (byte-and b (byte 1 1 1 1 1 0 1 1))
      (byte-or b (byte 0 0 0 0 0 1 0 0))))

(define (byte-set-seventh b bit)
  (if (zero? bit)
      (byte-and b (byte 1 1 1 1 1 1 0 1))
      (byte-or b (byte 0 0 0 0 0 0 1 0))))

(define (byte-set-eighth b bit)
  (if (zero? bit)
      (byte-and b (byte 1 1 1 1 1 1 1 0))
      (byte-or b (byte 0 0 0 0 0 0 0 1))))

(define byte-setters
  (immutable-vector
   byte-set-first
   byte-set-second
   byte-set-third
   byte-set-fourth
   byte-set-fifth
   byte-set-sixth
   byte-set-seventh
   byte-set-eighth))

(define/name (byte.bit pos)
  (make-lens (λ (b) (byte-ref b pos)) (vector-ref byte-setters pos)
             #:name enclosing-function-name))

(module+ test
  (test-case (name-string byte.bit)
    (define byte.third-bit (byte.bit 2))
    (check-equal? (lens-get byte.third-bit (byte 0 0 1 0 0 0 0 0)) 1)
    (check-equal? (lens-get byte.third-bit (byte 0 0 0 0 0 0 0 0)) 0)
    (check-equal?
     (lens-set byte.third-bit (byte 1 1 1 1 0 0 0 0) 0) (byte 1 1 0 1 0 0 0 0))
    (check-equal?
     (lens-set byte.third-bit (byte 1 1 1 1 0 0 0 0) 1)
     (byte 1 1 1 1 0 0 0 0))))
