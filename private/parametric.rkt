#lang racket/base

(require (for-syntax math/flonum
                     racket/base
                     racket/match
                     racket/stxparam)
         math/flonum
         racket/stxparam
         racket/match
         "./base-types.rkt")

(provide rgb->linear-rgb/1 rgb->linear-rgb/3 rgb->linear-rgb/4
         rgb->linear-rgb/5 rgb->linear-rgb/7)

;; Various procedures to linearize RGB space according to parametric curve types defined
;; in the standard ICC.1:2010 (Profile version 4.3.0.0)
;; http://www.color.org/specification/ICC1v43_2010-12.pdf (page 69)

(define-syntax-rule (norm a) (fl/ (fl a) 255.))

(define-syntax-rule (clamp a) (if (fl< a 0.) 0. (if (fl> a 1.) 1. a)))

(define-syntax-parameter _component #f)

(define-syntax (define-trc-proc stx)
  (syntax-case stx ()
    [(_ (id clr arg ...) expr) #'(define-trc-proc (id clr arg ...) () expr)]
    [(_ (id clr arg ...) (bind ...) expr)
     (andmap identifier? (syntax-e #'(id arg ...)))
     (with-syntax ([(comp ...) (generate-temporaries (list #'clr #'clr #'clr))])
       #'(define (id clr arg ...)
           (let (bind ...)
             (match clr
               [(rgb-space comp ...)
                (rgb/linear
                 (let* ([comp (norm comp)]
                        [comp (clamp comp)]
                        [comp
                         (syntax-parameterize ([_component (make-rename-transformer #'comp)])
                           expr)])
                   (clamp comp)) ...
                 clr)]
               [_ (raise-argument-error 'id "rgb-space?" 0 clr arg ...)]))))]))

(define-trc-proc (rgb->linear-rgb/1 x g)
  (flexpt _component g))

(define-trc-proc (rgb->linear-rgb/3 x g a b)
  ([t (fl/ (fl- b) a)])
  (if (fl>= _component t)
      (flexpt (fl+ (fl* a _component) b) g)
      0.0))

(define-trc-proc (rgb->linear-rgb/4 x g a b c)
  ([t (fl/ (fl- b) a)])
  (if (fl>= _component t)
      (fl+ (flexpt (fl+ (fl* a _component) b) g) c)
      c))

(define-trc-proc (rgb->linear-rgb/5 x g a b c d)
  (if (fl>= _component d)
      (flexpt (fl+ (fl* a _component) b) g)
      (fl* c _component)))

(define-trc-proc (rgb->linear-rgb/7 x g a b c d e f)
  (if (fl>= _component d)
      (fl+ (flexpt (fl+ (fl* a _component) b) g) c)
      (fl+ (fl* c _component) f)))
