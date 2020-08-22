#lang racket/base

(require (for-syntax racket/base
                     racket/match)
         math/flonum
         racket/match
         racket/pretty
         racket/string)

(provide define* dbg
         clamp clamp255 norm/clamp unnorm/clamp
         flremainder flmodulo
         zipwith
         ->fl*)


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

(define (clamp a)
  (if (fl< a 0.) 0. (if (fl> a 1.) 1. a)))

(define (clamp255 a)
  (if (< a 0) 0 (if (> a 255) 255 a)))

(define (norm/clamp a)
  (let* ([a (fl/ (fl a) 255.)])
    (clamp a)))

(define (unnorm/clamp a)
  (let* ([a (inexact->exact (round (fl* a 255.0)))])
    (clamp255 a)))

;; Racket's `remainder' operation extended to flonum.
;; Sign of the result is the same as the divident (first argument)
;; Similar to C99, C++, JavaScript '%' operator
(define (flremainder a n)
  (let ([a (fl a)] [n (fl n)])
    (fl- a (fl* n (fltruncate (fl/ a n))))))

;; Racket's `modulo' operation extended to flonum.
;; Sign of the result is the same as the divisor (second argument)
(define (flmodulo a n)
  (let ([a (fl a)] [n (fl n)])
    (fl- a (fl* n (flfloor (fl/ a n))))))

;; more like map. but for lists of uneven lengths
(define (zipwith f l1 l2)
  (let loop ([l1 l1] [l2 l2])
    (cond
      [(or (null? l1) (null? l2)) '()]
      [else (cons (f (car l1) (car l2)) (loop (cdr l1) (cdr l2)))])))


(define-syntax-rule (->fl* a ...) (values (fl a) ...))
