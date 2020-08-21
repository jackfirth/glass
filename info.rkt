#lang info

(define collection "glass")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "glass")))

(define deps
  (list "base"
        "fancy-app"
        "rebellion"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
