#lang racket/base

(module doc racket/base

  (require racket/contract/base)
  
  (provide
   (contract-out
    [make-module-sharing-evaluator-factory
     (->* ()
          (#:private (listof module-path?)
           #:public (listof module-path?))
          (-> (-> any/c any)))]))

  (require rebellion/collection/list
           scribble/example)

  ;@----------------------------------------------------------------------------

  (define (make-module-sharing-evaluator-factory
           #:public [public-modules empty-list]
           #:private [private-modules empty-list])
    (define base-factory
      (make-base-eval-factory (append private-modules public-modules)))
    (λ ()
      (define evaluator (base-factory))
      (evaluator `(require ,@public-modules))
      evaluator)))
