#lang racket/base

(require (for-syntax racket/base
                     racket/match)
         racket/match)

(provide define*)

(define-syntax (define* stx)
  (syntax-case stx ()
    [(_ (id mclause mclauses ...) body0 body ...)
     (identifier? #'id)
     (with-syntax ([(occ ...) (generate-temporaries (syntax-e #'(mclause mclauses ...)))])
       #'(define (id occ ...)
           (match*/derived
            (occ ...) 'id
            ((mclause mclauses ...) body0 body ...))))]))
