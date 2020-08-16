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


(define-syntax (define-color->pcs->color stx)
  (syntax-parse stx
    [(_ srctype:id dsttype:id)
     (with-syntax
         ([name (format-id #'srctype "~a->~a" #'srctype #'dsttype)]
          [(tmp) (generate-temporaries (list #'srctype))]
          [src->pcs (format-id #'srctype "~a->xyz" #'srctype)]
          [pcs->dst (format-id #'dsttype "xyz->~a" #'dsttype)])
       #'(define (name tmp) (pcs->dst (src->pcs tmp))))]))


(define-syntax (define-color->color/table/1 stx)
  (syntax-parse stx
    [(_ clr:id ...)
     (with-syntax ([((fro to) ...) (combinations (syntax->list #'(clr ...)) 2)])
       #'(begin
           (define-color->pcs->color fro to)
           ...
           (define-color->pcs->color to fro)
           ...))]))

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

;; Generic rgb to other spaces. Accepts any rgb type defined with define-rgb-colorspace
(define-color->color/table (rgb)
  (rgb/srgb rgb/display-p3 rgb/prophoto rgb/rec2020 rgb/adobe luv lch lab lch/ab))

(define-color->color/table (luv lch lab lch/ab)
  (rgb rgb/srgb rgb/display-p3 rgb/prophoto rgb/rec2020 rgb/adobe))
