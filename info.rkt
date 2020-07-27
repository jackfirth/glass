#lang info

(define collection "glass")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "glass")))

(define deps
  (list "rebellion"
        "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
