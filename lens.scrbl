#lang scribble/manual

@(require (for-label glass/lens
                     (except-in racket/base pair?)
                     racket/contract/base
                     rebellion/base/pair
                     rebellion/base/symbol
                     rebellion/binary/bit
                     rebellion/binary/byte
                     rebellion/collection/entry
                     rebellion/type/tuple)
          (submod glass/private/scribble-cross-document-tech doc)
          (submod glass/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'glass/lens
                   'rebellion/base/pair
                   'rebellion/binary/byte
                   'rebellion/collection/entry
                   'rebellion/type/tuple)
    #:private (list 'racket/base)))

@title{Lenses}
@defmodule[glass/lens]

A @deftech{lens} is a type of @tech{optic} for focusing on small parts of a
subject. A lens is built from a getter function, which extracts the focus from
the subject, and a setter function, which takes a subject and a replacement for
the focus and builds a new subject.

@defproc[(lens? [v any/c]) boolean?]{
 A predicate for @tech[#:key "lens"]{lenses}.}

@defproc[
 (make-lens
  [getter (-> any/c any/c)]
  [setter (-> any/c any/c any/c)]
  [#:name name (or/c interned-symbol? #f) #f])
 lens?]{
 Constructs a @tech{lens} named @racket[name] that focuses on subjects by
 calling @racket[getter] on the subject and updates the focus by calling
 @racket[setter] with the subject and the replacement focus.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-tuple-type point (x y))
    (define point.x
      (make-lens point-x (λ (p x) (point x (point-y p))) #:name 'point.x)))

   (lens-get point.x (point 4 7))
   (lens-set point.x (point 4 7) 100))}

@defproc[(lens-get [lens lens?] [subject any/c]) any/c]{
 Returns the focus of @racket[lens] on @racket[subject].}

@defproc[(lens-set [lens lens?] [subject any/c] [replacement any/c]) any/c]{
 Updates @racket[subject] with @racket[lens] by replacing its focus with
 @racket[replacement], returning an updated subject.}

@defproc[(lens-map [lens lens?] [subject any/c] [mapper (-> any/c any/c)])
         any/c]{
 Updates @racket[subject] with @racket[lens] by applying @racket[mapper] to its
 focus, returning an updated subject with the mapped focus.

 @(examples
   #:eval (make-evaluator) #:once
   (lens-map entry.value (entry 'a 16) sqrt))}

@defproc[(lens/c [subject-contract contract?] [focus-contract contract?])
         contract?]{
 A @reference-tech{contract combinator} for @tech[#:key "lens"]{lenses}. Creates
 a contract that accepts lenses whose subjects are checked with
 @racket[subject-contract] and whose foci are checked with
 @racket[focus-contract].}

@defthing[identity-lens lens?]{
 The identity @tech{lens}, which focuses on the entire subject and replaces it
 entirely when given a new focus.

 @(examples
   #:eval (make-evaluator) #:once
   (lens-get identity-lens 5)
   (lens-set identity-lens 5 100))}

@defproc[(lens-pipe [lens lens?] ...) lens?]{
 Joins each @racket[lens] to the next, building a composed lens that focuses on
 subjects by recursively focusing on the subject once with each lens from left
 to right. If only one lens is given, it is returned unchanged, and if no lenses
 are given, @racket[identity-lens] is returned.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt (define entry.key.first (lens-pipe entry.key pair.first)))
   (lens-get entry.key.first (entry (pair 'a 'c) 5))
   (lens-set entry.key.first (entry (pair 'a 'c) 5) 'f))}

@deftogether[[
 @defthing[pair.first (lens/c pair? any/c)]
 @defthing[pair.second (lens/c pair? any/c)]]]{
 Lenses that focus on the first and second values of a @rebellion-tech{pair},
 respectively.

 @(examples
   #:eval (make-evaluator) #:once
   (lens-get pair.first (pair 4 8))
   (lens-set pair.second (pair 4 8) 100))}

@deftogether[[
 @defthing[entry.key (lens/c entry? any/c)]
 @defthing[entry.value (lens/c entry? any/c)]]]{
 Lenses that focus on the key and value of an @rebellion-tech{entry},
 respectively.

 @(examples
   #:eval (make-evaluator) #:once
   (lens-get entry.key (entry 'a 1))
   (lens-set entry.value (entry 'a 1) 5))}

@defproc[(byte.bit [position (integer-in 0 7)]) (lens/c byte? bit?)]{
 Constructs a @tech{lens} that focuses on the bit at @racket[position] in a
 byte, with bit positions numbered from left to right.

 @(examples
   #:eval (make-evaluator) #:once
   (lens-get (byte.bit 7) (byte 1 1 1 1 1 1 1 0))
   (lens-set (byte.bit 7) (byte 0 0 0 0 0 0 0 0) 1))}
