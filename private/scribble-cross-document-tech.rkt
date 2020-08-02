#lang racket/base

(module doc racket/base

  (require racket/contract/base)

  (provide
   (contract-out
    [reference-tech cross-document-tech-function/c]
    [rebellion-tech cross-document-tech-function/c]))

  (require rebellion/base/immutable-string
           scribble/base
           scribble/core
           scribble/decode
           scribble/manual)

  ;@----------------------------------------------------------------------------

  (define cross-document-tech-function/c
    (->* ()
         (#:key (or/c string? #f) #:normalize? boolean?)
         #:rest (listof pre-content?)
         element?))

  (define ((cross-document-tech-function doc)
           #:key [key #f] #:normalize? [normalize? #t] . text)
    (apply tech
           #:doc doc
           #:key key
           #:normalize? normalize?
           text))

  (define reference-tech
    (cross-document-tech-function
     '(lib "scribblings/reference/reference.scrbl")))

  (define rebellion-tech
    (cross-document-tech-function '(lib "rebellion/main.scrbl"))))
