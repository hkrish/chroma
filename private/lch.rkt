#lang racket/base

(require math/flonum
         racket/math
         "./base-types.rkt"
         "./helpers.rkt"
         "./lab.rkt"
         "./luv.rkt")

(provide (all-defined-out))

;; LCH comes in two varieties
;;  - LCHuv is a cylidrical transformation of Luv color-space
;;  - LCHab is a cylidrical transformation of Lab color-space
;;
;; In chroma, LCHuv is refered to as lch. And LCHab is a derived struct type lch/ab.
;;
;; To convert between LCHuv and LCHab we need to go through the profile connection space
;; (XYZ). See conversions.rkt

(define* (luv->lch (luv l u v))
  (let-values ([(l u v) (->fl* l u v)])
    (let ([h (radians->degrees (atan v u))])
      (lch l (flhypot u v) (if (fl>= h 0.0) h (fl+ h 360.))))))

(define* (lab->lch/ab (lab l a b))
  (let-values ([(l a b) (->fl* l a b)])
    (let ([h (radians->degrees (atan b a))])
      (lch/ab l (flhypot a b) (if (fl>= h 0.0) h (fl+ h 360.))))))

(define* (lch->luv (lch l c h))
  (let ([h (degrees->radians (fl h))]
        [c (fl c)])
    (luv (fl l) (fl* c (flcos h)) (fl* c (flsin h)))))

(define* (lch/ab->lab (lch/ab l c h))
  (let ([h (degrees->radians (fl h))]
        [c (fl c)])
    (lab (fl l) (fl* c (flcos h)) (fl* c (flsin h)))))

(define (lch->xyz c) (luv->xyz (lch->luv c)))

(define (xyz->lch c) (luv->lch (xyz->luv c)))

(define (lch/ab->xyz c) (lab->xyz (lch/ab->lab c)))

(define (xyz->lch/ab c) (lab->lch/ab (xyz->lab c)))

(struct lch color (l c h)
  #:transparent
  #:property prop:color->xyz lch->xyz
  #:methods gen:custom-write
  [(define write-proc color-printer)])

(struct lch/ab lch ()
  #:property prop:color->xyz lch/ab->xyz
  #:transparent)
