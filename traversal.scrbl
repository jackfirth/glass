#lang scribble/manual

@(require (for-label glass/lens
                     glass/prism
                     glass/traversal
                     racket/base
                     racket/contract/base
                     racket/list
                     racket/math
                     racket/sequence
                     rebellion/base/result
                     rebellion/base/symbol
                     rebellion/collection/entry
                     rebellion/collection/immutable-vector
                     rebellion/collection/list
                     rebellion/type/tuple)
          (submod glass/private/scribble-cross-document-tech doc)
          (submod glass/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'glass/lens
                   'glass/prism
                   'glass/traversal
                   'racket/list
                   'rebellion/base/result
                   'rebellion/collection/entry
                   'rebellion/collection/immutable-vector
                   'rebellion/type/tuple)
    #:private (list 'racket/base)))

@title{Traversals}
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

@defproc[(make-traversal
          [#:getter getter (-> any/c immutable-vector?)]
          [#:setter setter (-> any/c immutable-vector? any/c)]
          [#:counter counter (-> any/c natural?)
           (λ (suject) (immutable-vector-length (getter subject)))]
          [#:name name (or/c interned-symbol? #f) #f])
         traversal?]{
 Constructs a @tech{traversal} named @racket[name].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define-tuple-type player (name x y))
    (define player-coordinates
      (make-traversal
       #:getter (λ (p) (immutable-vector (player-x p) (player-y p)))
       #:setter
       (λ (p xy) (player (player-name p) (vector-ref xy 0) (vector-ref xy 1)))
       #:counter (λ (_) 2)
       #:name 'player-coordinates)))
   (traversal-get-all player-coordinates (player "Catherine" 2 7))
   (traversal-set-all player-coordinates (player "Catherine" 2 7) (list 0 0)))}

@defproc[(traversal/c [subject-contract contract?] [foci-contract contract?])
         contract?]{
 A @reference-tech{contract combinator} for @tech{traversals}. Creates a
 contract that accepts traversals whose subjects are checked with
 @racket[subject-contract] and whose foci are checked with
 @racket[foci-contract].}

@section{Using Traversals}

@defproc[(traversal-get-all [traversal traversal?] [subject any/c])
         immutable-vector?]{
 Traverses @racket[subject] with @racket[traversal] and returns a list of the
 traversal's foci.}

@defproc[(traversal-set-all
          [traversal traversal?] [subject any/c] [foci (sequence/c any/c)])
         any/c]{
 Traverses @racket[subject] with @racket[traversal] and replaces each focus with
 an element of @racket[foci], returning a new subject. If @racket[foci] does not
 contain the same number of elements as the traversed subject, a contract error
 is raised.}

@defproc[(traversal-count [traversal traversal?] [subject any/c]) natural?]{
 Traverses @racket[subject] with @racket[traversal] and counts the traversal's
 foci.}

@defproc[(traversal-map
          [traversal traversal?] [subject any/c] [mapper (-> any/c any/c)])
         any/c]{
 Traverses @racket[subject] with @racket[traversal] and updates each focus with
 @racket[mapper], returning a new subject with the updated foci.

 @(examples
   #:eval (make-evaluator) #:once
   (traversal-map string-traversal "hello" char-upcase))}

@defproc[(traversal-clear
          [traversal traversal?] [subject any/c] [replacement any/c])
         any/c]{
 Traverses @racket[subject] with @racket[traversal] and sets each focus to
 @racket[replacement], returning a new subject with the updated foci.

 @(examples
   #:eval (make-evaluator) #:once
   (traversal-clear string-traversal "hello" #\x))}

@defproc[(traversal-reverse [traversal traversal?] [subject any/c]) any/c]{
 Traverses @racket[subject] with @racket[traversal] and reverses the order of
 the traversed foci, returning a new subject with the reversed foci.

 @(examples
   #:eval (make-evaluator) #:once
   (traversal-reverse string-traversal "hello"))}

@section{Predefined Traversals}

@defthing[list-traversal (traversal/c list? any/c)]{
 A @tech{traversal} that traverses the elements of a list.}

@defthing[vector-traversal (traversal/c vector? any/c)]{
 A @tech{traversal} that traverses the elements of a vector. The traversal
 accepts both mutable and immutable vectors, but it only produces immutable
 vectors.}

@defthing[string-traversal (traversal/c string? char?)]{
 A @tech{traversal} that traverses the characters of a string. The traversal
 accepts both mutable and immutable strings, but it only produces immutable
 strings.

 @(examples
   #:eval (make-evaluator) #:once
   (traversal-count string-traversal "hello")
   (traversal-get-all string-traversal "hello"))}

@defthing[identity-traversal traversal?]{
 The identity @tech{traversal}. Traverses only one element of the subject, and
 that element is the subject itself.

 @(examples
   #:eval (make-evaluator) #:once
   (traversal-get-all identity-traversal 6)
   (traversal-set-all identity-traversal 6 (list 100)))}

@section{More Traversal Constructors}

@defproc[(traversal-pipe [traversal traversal?] ...) traversal?]{
 Joins each @racket[traversal] to the next, building a composed traversal that
 traverses subjects by traversing the subject with the first traversal, then
 traversing each of those traversed elements with the second traversal, and so
 on for each traversal in left-to-right order. If only one @racket[traversal] is
 given, it is returned directly. If no traversals are given,
 @racket[identity-traversal] is returned.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define list-of-strings-traversal
      (traversal-pipe list-traversal string-traversal))
    (define strings (list "hello" "darkness" "my" "old" "friend")))
   (traversal-map list-of-strings-traversal strings char-upcase)
   (traversal-clear list-of-strings-traversal strings #\-)
   (traversal-reverse list-of-strings-traversal strings))}

@defproc[(subtraversal
          [traversal traversal?]
          [inclusive-start natural?]
          [exclusive-end (or/c natural? #f) #f])
         traversal?]{
 Limits @racket[traversal] to only operate on the elements between
 @racket[inclusive-start] and @racket[exclusive-end]. If @racket[exclusive-end]
 is false, the subtraversal includes all elements after
 @racket[inclusive-start].

 @(examples
   #:eval (make-evaluator) #:once
   (traversal-clear (subtraversal string-traversal 2 8) "hello world" #\_))}

@defproc[(lens->traversal [lens lens?]) traversal?]{
 Converts @racket[lens] into a @tech{traversal} that always focuses on exactly
 one part of the subject using @racket[lens].

 @(examples
   #:eval (make-evaluator) #:once
   (define entry.key-traversal (lens->traversal entry.key))
   (traversal-get-all entry.key-traversal (entry 'grapes 4))
   (traversal-set-all entry.key-traversal (entry 'grapes 4) (list 'apples)))}

@defproc[(prism->traversal [prism prism?]) traversal?]{
 Converts @racket[prism] into a @tech{traversal} that has either zero foci, if
 @racket[prism] does not match the subject, or one focus if @racket[prism] does
 match the subject.

 @(examples
   #:eval (make-evaluator) #:once
   (define success-traversal (prism->traversal success-prism))
   (traversal-get-all success-traversal (success 5))
   (traversal-set-all success-traversal (success 5) (list 10)))}
