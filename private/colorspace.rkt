#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/list
                     syntax/parse
                     math/flonum)
         racket/match
         racket/contract
         racket/generic
         "./base-types.rkt"
         "./parametric.rkt"
         "./matrix3.rkt"
         "./helpers.rkt")

;; RGB --------------------


(define-values (prop:rgb<->xyz rgb<->xyz? rgb<->xyz-ref)
  (make-struct-type-property 'rgb<->xyz))

(define/contract (check-reference-white rgbi si)
  (-> (or/c 'D50 'D55 'D65 'D75 xyz?) any/c any/c)
  (if (xyz? rgbi) rgbi (hash-ref ref-whites rgbi)))

(define-values (prop:reference-white reference-white? reference-white-ref)
  (make-struct-type-property 'reference-white check-reference-white))

(define-syntax-rule (err/nparam stx)
    (raise-syntax-error
     #f
     "unknown icc tone reproduction curve type.\n  expected: 1, 3, 4, 5, or 7 arguments"
     stx))

(begin-for-syntax
  (define (get-trc-fn stx)
    (syntax-case stx ()
      [(g) #'rgb->linear-rgb/1]
      [(g a b) #'rgb->linear-rgb/3]
      [(g a b c) #'rgb->linear-rgb/4]
      [(g a b c d) #'rgb->linear-rgb/5]
      [(g a b c d e f) #'rgb->linear-rgb/7]
      [_
       (raise-syntax-error
        'define-rgb-space
        "unknown icc tone reproduction curve type.\n  expected: 1, 3, 4, 5, or 7 arguments"
        stx)]))

  (define-syntax-class primary
    #:literals [xyz]
    #:datum-literals [xy]
    (pattern (xyz x:expr y:expr z:expr)
             #:attr type:xyz #t
             #:attr xyz #'(x y z))
    (pattern (xy x:number y:number)
             #:attr type:xyz #f
             #:attr xyz (let ([x1 (syntax->datum #'x)]
                              [y1 (syntax->datum #'y)])
                          (datum->syntax #'x (list (/ x1 y1) 1.0 (/ (- 1.0 x1 y1) y1)))))
    (pattern (xy x:expr y:expr)
             #:attr type:xyz #f
             #:attr xyz #'((/ x y) 1.0 (/ (- 1.0 x y) y)))))

;; We need - reference-white (illuminant)
;;         - rgb primaries
;;         - TRC parameters
;; Defines
;; - rgb/name
;; - rgb/name->xyz/pcs
;; - xyz/pcs->rgb/name
(define-syntax (define-rgb-space stx)
  (syntax-parse stx
    [(_ name:id
        (~alt (~once (~optional (~seq #:reference-white refw:expr) #:defaults ([refw #''D65])))
              (~once (~seq #:red-primary rp:primary))
              (~once (~seq #:green-primary gp:primary))
              (~once (~seq #:blue-primary bp:primary))
              (~once (~seq #:trc (trc-args:expr ...))))
        ...)
     (define/with-syntax id (format-id #'name "rgb/~a" #'name))
     (define/with-syntax id->xyz/pcs (format-id #'name "rgb/~a->xyz/pcs" #'name))
     (define/with-syntax xyz/pcs->id (format-id #'name "xyz/pcs->rgb/~a" #'name))
     (define/with-syntax (tmp tmat rgbm rgbmi tr tg tb tx ty tz sr sg sb)
       (generate-temporaries (make-list 13 #'name)))
     (define/with-syntax rgbmat (with-syntax ([(xr yr zr) #'rp.xyz]
                                              [(xg yg zg) #'gp.xyz]
                                              [(xb yb zb) #'bp.xyz])
                                  #'(make-3x3 xr xg xb yr yg yb zr zg zb)))
     (define/with-syntax trcfn (get-trc-fn #'(trc-args ...)))
     #''(begin
          (struct id rgb-space () #:transparent
            #:property prop:rgb<->xyz (list id->xyz/pcs))

          (define id->xyz/pcs
            (let* ([rgbm rgbmat]
                   [rgbmi (3x3-inverse rgbm)]
                   [(sr sg sb) (hash-ref ref-whites)]
                   [(sr sg sb) (3x3*vec rgbmi xrw yrw zrw)])
              (lambda (tmp)
                (match tmp
                  [(rgb-space tr tg tb)
                   (xyz tx ty tz)])))
            )

          rgbmat
          (trcfn clr-arg trc-args ...)
          )]))

(define/match (rgbmat refw rp gp bp)
  [((xyz xrw yrw zrw) (xyz xr yr zr) (xyz xg yg zg) (xyz xb yb zb))
   (let*-values
       ([(rgbm) (make-3x3 xr xg xb yr yg yb zr zg zb)]
        [(rgbmi) (3x3-inverse rgbm)]
        [(sr sg sb) (3x3*vec rgbmi xrw yrw zrw)])
     (3x3-mult-columns rgbm sr sg sb))])

(define rw (hash-ref ref-whites 'D50))
(define rp (xyz 0.436 0.222 0.014))
(define gp (xyz 0.385 0.717 0.097))
(define bp (xyz 0.143 0.061 0.714))
(rgbmat rw rp gp bp)

(define/match (xyz-adapt-D50->D65 s)
  [((xyz x y z))
   (let-values ([(x y z) (3x3*vec adapt-D50->D65 x y z)])
     (xyz x y z))])

(define/match (xyz-adapt-D65->D50 s)
  [((xyz x y z))
   (let-values ([(x y z) (3x3*vec adapt-D65->D50 x y z)])
     (xyz x y z))])

(define-rgb-space srgb
  #:red-primary (xyz 1. 1. 1.)
  #:green-primary (xyz 2. 2. 2.)
  #:blue-primary (xyz 3. 3. 3.)
  #:trc (2.2))

(define-rgb-space srgb2
  #:red-primary (xy 0.64 0.33)
  #:green-primary (xy 0.30 0.60)
  #:blue-primary (xy 0.15 0.06)
  #:trc (2.2))
