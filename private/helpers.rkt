#lang racket/base

(require (for-syntax racket/base
                     racket/match)
         racket/match
         racket/pretty
         racket/string)

(provide define* dbg)

(define-syntax (define* stx)
  (syntax-case stx ()
    [(_ (id mclause mclauses ...) body0 body ...)
     (identifier? #'id)
     (with-syntax ([(occ ...) (generate-temporaries (syntax-e #'(mclause mclauses ...)))])
       (syntax/loc stx
         (define (id occ ...)
           (match*/derived
            (occ ...) (id mclause mclauses ...)
            ((mclause mclauses ...) body0 body ...)))))]))


(define-syntax (dbg stx)
  (syntax-case stx ()
    [(_ f)
     #`(call-with-values
        (lambda () f)
        (lambda res
          (display "  : ")
          (pretty-print 'f)
          (displayln
           (let ([lines (map (lambda (r) (pretty-format r #:mode 'print)) res)])
             (string-join lines "     \n" #:before-first "  -> ")))
          (apply values res)))]))
