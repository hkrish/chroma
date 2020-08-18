#lang racket/base

(require (for-syntax math/flonum
                     racket/base)
         math/flonum
         racket/match
         racket/struct
         "./helpers.rkt"
         "./matrix3.rkt")

(provide (struct-out color)
         (struct-out xyz)
         (struct-out xyY)
         (struct-out rgb-space)
         xyY->xyz
         xyz->xyY
         prop:color->xyz color->xyz? color->xyz-ref
         color->xyz
         illuminant/pcs
         reference-white->xyz
         ca/bradford
         ca-inverse/bradford
         make-chromatic-adaptation-matrix
         adapt-D50->D65
         adapt-D65->D50
         xyz/D65->xyz/D50
         xyz/D50->xyz/D65
         current-color-print-round-places
         color-printer)

;; The naming of different color types are `color-space-type/color-profile-id'
;;  - color-space-types as defined by CIE (RGB, Luv, Lab, XYZ etc.) except always in
;;    lowercase.
;;  - color-profile-ids as defined by ICC and other standards, and various manufacturers
;;    (sRGB, Display-P3, ProPhoto etc.) except always in lowercase.
;;
;; TODO: explain: rgb is an alias for rgb/srgb.

;; ----------------------------------------
;; Helpers for printing

(define current-color-print-round-places (make-parameter #f))

(define (color-type-name c)
  (if (color? c)
      (let-values ([(t _) (struct-info c)])
        (if (struct-type? t)
            (let-values ([(n _a _b _c _d _e _f _g) (struct-type-info t)])
              n)
            'color))
      #f))

(define (rounded-component-list v)
  (define m (let ([m (current-color-print-round-places)])
              (and (number? m) (flexpt 10.0 (fl m)))))
  (define (rnd v) (if (and m (inexact? v)) (/ (flround (fl* v m)) m) v))
  (match v
    [(rgb-space r g b) (list (rnd r) (rnd g) (rnd b))]
    [(xyz x y z) (list (rnd x) (rnd y) (rnd z))]
    [(xyY x y Y) (list (rnd x) (rnd y) (rnd Y))]
    [_ (if (color? v)
           (for/list ([c (in-list (cdr (vector->list (struct->vector v))))])
             (rnd c))
           (raise-argument-error 'color-printer "color?" v))]))

(define color-printer
  (make-constructor-style-printer
   (lambda (o) (color-type-name o))
   (lambda (o) (rounded-component-list o))))


;; ----------------------------------------
;; Definitions

(define-values (prop:color->xyz color->xyz? color->xyz-ref)
  (make-struct-type-property 'color->xyz))

;; interpreted in sRGB space if no input color space is specified. Base type for all
;; colours
(struct color () #:transparent)

;; CIEXYZ color space
(struct xyz color (x y z) #:transparent
  #:property prop:color->xyz (lambda (x) x)
  #:methods gen:custom-write
  [(define write-proc color-printer)])

(define* (xyz->xyY (xyz x y z))
  (let ([x (fl x)]
        [y (fl y)]
        [z (fl z)])
    (let ([c (fl/ 1.0 (fl+ x y z))])
      (xyY (fl* c x) (fl* c y) (fl- 1.0 x y)))))

(define* (xyY->xyz (xyY x y Y))
  (let ([y/Y (fl/ Y y)])
    (xyz (fl* y/Y x) Y (fl* y/Y (fl- 1.0 x y)))))

;; CIExyY colorspace. Y is the same Y as in XYZ.
;; Commonly used in specifications.
(struct xyY color (x y Y) #:transparent
  #:guard (lambda (x y Y name) (values (fl x) (fl y) (fl Y)))
  #:property prop:color->xyz xyY->xyz
  #:methods gen:custom-write
  [(define write-proc color-printer)])

;; Base type for RGB input spaces. Derived types are typically companded with
;; a per-channel tone reproduction curve
(struct rgb-space color (r g b) #:transparent
  #:methods gen:custom-write
  [(define write-proc color-printer)])

;; Generic rgb->xyz function.
(define (color->xyz r)
  (cond
    [(color->xyz? r) ((color->xyz-ref r) r)]
    [else
     (raise-argument-error 'color->xyz "colorspace with `color->xyz' property" r)]))


;; ----------------------------------------
;; Predefined constants

;; xyz coordinates of some of the standard CIE illuminants
(define (reference-white->xyz rw)
  (cond
    [(eq? 'D50 rw) (xyz 0.96420 1. 0.82491)] ;; (xyz 0.96422 1. 0.82521)
    [(eq? 'D65 rw) (xyz 0.95047 1. 1.08883)]
    [(eq? 'D55 rw) (xyz 0.95682 1. 0.92149)]
    [(eq? 'D75 rw) (xyz 0.94972 1. 1.22638)]
    [(eq? 'C rw)   (xyz 0.98074 1. 1.18232)]
    [(xyz? rw)     rw]
    [else (raise-argument-error 'reference-white->xyz "(or/c xyz? 'D50 'D55 'D65 'D75)" rw)]))

;; Bradford chromatic adaptation matrix
(define ca/bradford (make-3x3 0.8951  0.2664 -0.1614
                              -0.7502  1.7135  0.0367
                              0.0389 -0.0685  1.0296))

(define ca-inverse/bradford (3x3-inverse ca/bradford))

;; Illuminant for profile-connection-space (internally xyz is represented in 'D50)
(define illuminant/pcs (reference-white->xyz 'D50))

(define-syntax-rule (make-chromatic-adaptation-matrix src-ref-white dst-ref-white)
  (let ([srcrw (if (xyz? src-ref-white) src-ref-white (reference-white->xyz src-ref-white))]
        [dstrw (if (xyz? dst-ref-white) dst-ref-white (reference-white->xyz dst-ref-white))])
    (match* (srcrw dstrw)
      [((xyz s1 s2 s3) (xyz d1 d2 d3))
       (let*-values ([(s1 s2 s3) (->fl* s1 s2 s3)]
                     [(s1 s2 s3) (3x3*vec ca/bradford s1 s2 s3)]
                     [(d1 d2 d3) (->fl* d1 d2 d3)]
                     [(d1 d2 d3) (3x3*vec ca/bradford d1 d2 d3)])
         (3x3-snap-to-identity
          (3x3* ca-inverse/bradford
                (3x3* (3x3-diag (fl/ d1 s1) (fl/ d2 s2) (fl/ d3 s3)) ca/bradford))
          #:tolerance 1e-10))])))

;; Some commonly used chromatic adaptation matrices

(define adapt-D50->D65 (make-chromatic-adaptation-matrix 'D50 'D65))

(define adapt-D65->D50 (make-chromatic-adaptation-matrix 'D65 'D50))

(define* (xyz/D50->xyz/D65 (xyz x y z))
  (let-values ([(x y z) (3x3*vec adapt-D50->D65 x y z)])
    (xyz x y z)))

(define* (xyz/D65->xyz/D50 (xyz x y z))
  (let-values ([(x y z) (3x3*vec adapt-D65->D50 x y z)])
    (xyz x y z)))


