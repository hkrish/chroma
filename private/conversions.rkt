#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/list
                     syntax/parse)
         "./base-types.rkt"
         "./rgb.rkt"
         "./luv.rkt"
         "./lab.rkt"
         "./lch.rkt")

(provide (except-out (all-defined-out)
                     define-color->pcs->color
                     define-color->color/table))


(define-syntax (define-color->pcs->color stx)
  (syntax-parse stx
    [(_ srctype:id dsttype:id)
     (with-syntax
         ([name (format-id #'srctype "~a->~a" #'srctype #'dsttype)]
          [(tmp) (generate-temporaries (list #'srctype))]
          [src->pcs (format-id #'srctype "~a->xyz" #'srctype)]
          [pcs->dst (format-id #'dsttype "xyz->~a" #'dsttype)])
       #'(define (name tmp) (pcs->dst (src->pcs tmp))))]))

(define-syntax (define-color->color/table stx)
  (syntax-parse stx
    [(_ (src:id ...)) #'(define-color->color/table (src ...) (src ...))]
    [(_ (src:id ...) (dst:id ...))
     (let* ([combs (cartesian-product (syntax->list #'(src ...))
                                      (syntax->list #'(dst ...)))]
            [combs (filter-not (lambda (x) (bound-identifier=? (car x) (cadr x))) combs)])
       (with-syntax ([((fro to) ...) combs])
         #'(begin
             (define-color->pcs->color fro to)
             ...)))]))

;; Generic color to other colorspace conversion. Accepts any color type with
;; prop:color->xyz property

(define-color->color/table (color)
  (rgb rgb/srgb rgb/display-p3 rgb/prophoto rgb/rec2020 rgb/adobe luv lch lab lch/ab))
