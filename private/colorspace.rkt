#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
         racket/unsafe/ops
         "./base-types.rkt"
         "./matrix3.rkt"
         "./parametric.rkt")

(provide define-rgb-colorspace
         prop:rgb<->xyz rgb<->xyz? rgb<->xyz-ref
         prop:reference-white reference-white? reference-white-ref)


(define-values (prop:rgb<->xyz rgb<->xyz? rgb<->xyz-ref)
  (make-struct-type-property 'rgb<->xyz))

(define-values (prop:reference-white reference-white? reference-white-ref)
  (make-struct-type-property 'reference-white))

(define-syntax-rule (prim->xyz name p)
  (cond
    [(xyz? p) (values (xyz-x p) (xyz-y p) (xyz-z p))]
    [(xyY? p) (let ([x (xyY-x p)]
                    [y (xyY-y p)]
                    [Y (xyY-Y p)])
                (let ([Y/y (/ Y y)])
                  (values (* x Y/y) Y (* (- 1. x y) Y/y))))]
    [else (raise-argument-error
           'define-rgb-colorspace (format "(or/c xyz? xyY?) for option `~a'" name) p)]))

(define (primaries->matrix rp gp bp)
  (let-values
      ([(xr yr zr) (prim->xyz '#:red-primary rp)]
       [(xg yg zg) (prim->xyz '#:green-primary gp)]
       [(xb yb zb) (prim->xyz '#:blue-primary bp)])
    (make-3x3 xr xg xb yr yg yb zr zg zb)))

(begin-for-syntax
  (define (check-trc stx)
    (syntax-case stx ()
      [(g) #t]
      [(g a b) #t]
      [(g a b c) #t]
      [(g a b c d) #t]
      [(g a b c d e f) #t]
      [_ #f])))

;; TODO: Docs
(define-syntax (define-rgb-colorspace stx)
  (syntax-parse stx
    [(_ name:id
        (~alt (~once (~optional (~seq #:reference-white refw:expr) #:defaults ([refw #''D65])))
              (~once (~seq #:red-primary rp:expr))
              (~once (~seq #:green-primary gp:expr))
              (~once (~seq #:blue-primary bp:expr))
              (~once (~optional (~seq #:intent int:expr) #:defaults ([int #''perceptual])))
              (~once (~seq #:trc (trc-args:expr ...))))
        ...)
     (unless (check-trc #'(trc-args ...))
       (raise-syntax-error
        'define-rgb-colorspace
        "unknown icc tone reproduction curve type.\n  expected: 1, 3, 4, 5, or 7 arguments"
        stx #'(trc-args ...)))
     (define/with-syntax id (format-id #'name "rgb/~a" #'name))
     (define/with-syntax s:id (format-id #'name "struct:~a" #'id))
     (define/with-syntax id? (format-id #'name "~a?" #'id))
     (define/with-syntax id->xyz (format-id #'name "~a->xyz" #'id))
     (define/with-syntax xyz->id (format-id #'name "xyz->~a" #'id))
     (define/with-syntax msg/id? (format "~a" (syntax->datum #'id?)))
     (define/with-syntax (tmp rgbm rgbmi trefw tr tg tb tx ty tz sr sg sb)
       (generate-temporaries (make-list 13 #'name)))
     (define/with-syntax adapted
       (syntax-parse #'refw
         #:datum-literals (D50 D65)
         [(quote D50) #'rgbm]
         [(quote D65) #'(3x3* adapt-D65->D50 rgbm)]
         [_ #'(let ([adaptm (make-chromatic-adaptation-matrix trefw 'D50)])
                (3x3* adaptm rgbm))]))
     #'(define-values (s:id id id? id->xyz xyz->id)
         (let-values
             ([(id->xyz xyz->id)
               (let-values
                   ([(rgbm rgbmi)
                     (let*-values
                         ([(trefw) (reference-white->xyz refw)]
                          [(rgbm) (primaries->matrix rp gp bp)]
                          [(rgbmi) (3x3-inverse rgbm)]
                          [(sr sg sb) (values (xyz-x trefw) (xyz-y trefw) (xyz-z trefw))]
                          [(sr sg sb) (3x3*vec rgbmi sr sg sb)]
                          [(rgbm) (3x3-mult-columns rgbm sr sg sb)]
                          [(rgbm) adapted])
                       (values rgbm (3x3-inverse rgbm)))])
                 (values
                  (procedure-rename
                   (lambda (tmp)
                     (if (id? tmp)
                         (let ([tr (unsafe-struct-ref tmp 0)]
                               [tg (unsafe-struct-ref tmp 1)]
                               [tb (unsafe-struct-ref tmp 2)])
                           (let*-values
                               ([(tr tg tb) (expand-trc-func (tr tg tb trc-args ...))]
                                [(tx ty tz) (3x3*vec rgbm tr tg tb)])
                             (xyz tx ty tz)))
                         (raise-argument-error 'id->xyz msg/id? tmp)))
                   'id->xyz)
                  (procedure-rename
                   (lambda (tmp)
                     (if (xyz? tmp)
                         (let ([tx (unsafe-struct-ref tmp 0)]
                               [ty (unsafe-struct-ref tmp 1)]
                               [tz (unsafe-struct-ref tmp 2)])
                           (let*-values
                               ([(tr tg tb) (3x3*vec rgbmi tx ty tz)]
                                [(tr tg tb) (expand-trc-inverse-func (tr tg tb trc-args ...))])
                             (id tr tg tb)))
                         (raise-argument-error 'xyz->id "xyz?" tmp)))
                   'xyz->id)))])

           (struct id rgb-space () #:transparent
             #:property prop:reference-white (reference-white->xyz refw)
             #:property prop:rgb<->xyz (list id->xyz xyz->id))

           (values s:id id id? id->xyz xyz->id)))]))
