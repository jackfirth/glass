#lang scribble/manual

@(require (for-label glass
                     glass/lens
                     glass/prism
                     glass/traversal
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

@title{Glass: Composable Optics}
@defmodule[glass]

This is an experimental library for optics. An @deftech{optic} is an object for
viewing one form of data, called the @deftech{subject}, in some other form,
called a @deftech{focus}. Optics are bidirectional: in addition to viewing the
focus, optics allow @emph{changing} the focus to get a new, updated subject.
There are several different kinds of optics, each of which expresses a different
relationship between subject and focus:

@itemlist[
 @item{@tech[#:key "lens"]{Lenses} focus on one small part of the subject, such
  as a field of a struct. Other parts of the subject are left untouched when the
  focus is changed. For example, the @racket[entry.key] lens focuses on the key
  of an @rebellion-tech{entry} object, allowing you to change the key of an
  entry.}

 @item{@tech{Prisms} focus on one specific kind of subject, such as a subtype of
  a struct. Other kinds of subjects are ignored. For example,
  @racket[success-prism] focuses on successful @rebellion-tech{result} objects
  and ignores failed ones, allowing you to change the values inside only
  successful results.}

 @item{@tech{Traversals} focus on several parts of the subject at once, such as
  the characters of a string. Each focus can be updated to a new value, but a
  traversal cannot change the total number of foci.}]

I'm working on this library for fun as a hypothetical successor to the
@racketmodname[lens #:indirect] library. I might put more work into it, or I
might not. Absolutely no promise of backwards compatibility whatsoever. Caveat
emptor.

@section{Lenses}
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

@section{Prisms}
@defmodule[glass/prism]

A @deftech{prism} is a type of @tech{optic} for focusing on a specific kind of
subject and ignoring other kinds. A prism is built from a matching function,
which focuses on subjects of the correct kind, and a casting function, which
transforms a replacement focus back into the subject.

@defproc[(prism? [v any/c]) boolean?]{
 A predicate for @tech{prisms}.}

@section{Traversals}
@defmodule[glass/traversal]

A @deftech{traversal} is a type of @tech{optic} for focusing on several parts of
a subject at once. A traversal is built from a getter function, which extracts a
list of foci from the subject, and a setter function, which takes a subject and
a list of replacement foci and builds a new subject. Traversals are not allowed
to change the number of foci when replacing them: if a traversal's getter views
10 foci in a subject, then the traversal's setter will only accept lists of
exactly 10 replacement foci.

@defproc[(traversal? [v any/c]) boolean?]{
 A predicate for @tech{traversals}.}
