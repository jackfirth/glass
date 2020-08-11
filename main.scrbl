#lang scribble/manual

@(require (for-label glass
                     glass/lens
                     glass/prism
                     glass/traversal)
          (submod glass/private/scribble-cross-document-tech doc))

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

@include-section[(lib "glass/lens.scrbl")]
@include-section[(lib "glass/prism.scrbl")]
@include-section[(lib "glass/traversal.scrbl")]
