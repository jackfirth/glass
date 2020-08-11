#lang scribble/manual

@(require (for-label glass/prism
                     racket/base
                     racket/contract/base
                     rebellion/base/option
                     rebellion/base/result
                     rebellion/base/symbol)
          (submod glass/private/scribble-cross-document-tech doc)
          (submod glass/private/scribble-evaluator-factory doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'glass/prism
                   'rebellion/base/option
                   'rebellion/base/result)
    #:private (list 'racket/base)))

@title{Prisms}
@defmodule[glass/prism]

A @deftech{prism} is a type of @tech{optic} for focusing on a specific kind of
subject and ignoring other kinds. A prism is built from a matching function,
which focuses on subjects of the correct kind, and a casting function, which
transforms a replacement focus back into the subject.

@defproc[(prism? [v any/c]) boolean?]{
 A predicate for @tech{prisms}.}

@defproc[(make-prism
          [matcher (-> any/c option?)]
          [caster (-> any/c any/c)]
          [#:name name (or/c interned-symbol? #f) #f])
         prism?]{
 Constructs a @tech{prism} named @racket[name].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define number-string-prism
      (make-prism
       (λ (s)
         (define num (string->number s))
         (if num (present num) absent))
       number->string
       #:name 'number-string-prism)))

   (prism-match number-string-prism "124")
   (prism-match number-string-prism "elephant")
   (prism-cast number-string-prism 100))}

@defproc[(prism-match [prism prism?] [subject any/c]) option?]{
 Matches @racket[subject] against @racket[prism], returning an
 @rebellion-tech{option} that is present if @racket[prism] was able to extract a
 focus from @racket[subject] and absent otherwise.

 @(examples
   #:eval (make-evaluator) #:once
   (prism-match success-prism (success 123))
   (prism-match success-prism (failure "oh no!")))}

@defproc[(prism-cast [prism prism?] [focus any/c]) any/c]{
 Casts @racket[focus] back into a subject value using @racket[prism]. This is
 the inverse operation of @racket[prism-match] --- if @racket[prism-match]
 successfully extracts a value from a subject, that value can be converted back
 to the original subject using @racket[prism-cast].

 @(examples
   #:eval (make-evaluator) #:once
   (prism-cast success-prism 123))}
