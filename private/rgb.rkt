#lang racket/base

(require "./base-types.rkt"
         "./colorspace.rkt")

(provide (struct-out rgb/srgb) rgb/srgb->xyz xyz->rgb/srgb
         (struct-out rgb/display-p3) rgb/display-p3->xyz xyz->rgb/display-p3
         (struct-out rgb/prophoto) rgb/prophoto->xyz xyz->rgb/prophoto
         (struct-out rgb/adobe) rgb/adobe->xyz xyz->rgb/adobe
         (struct-out rgb/rec2020) rgb/rec2020->xyz xyz->rgb/rec2020
         rgb->xyz
         (rename-out [rgb/srgb rgb]
                     [rgb/srgb? rgb?]
                     [struct:rgb/srgb struct:rgb]
                     [xyz->rgb/srgb xyz->rgb]
                     [rgb-space-r rgb-r]
                     [rgb-space-g rgb-g]
                     [rgb-space-b rgb-b]))

;; Some well known RGB colorspaces

;; The sRGB colorspace. This is also exported as the default rgb space.
(define-rgb-colorspace srgb
  #:reference-white 'D65
  #:red-primary (xyY 0.64 0.33 1.)
  #:green-primary (xyY 0.30 0.60 1.)
  #:blue-primary (xyY 0.15 0.06 1.)
  #:trc (2.4 0.948 0.052 0.0774 0.0405))

;; Display-P3 color space. Default colorspace for macOS, iOS, ...
;;  Based of DCI-P3, but uses sRGB's Tone Reproduction Curve (TRC).
(define-rgb-colorspace display-p3
  #:reference-white 'D65
  #:red-primary (xyY 0.68 0.32 1.)
  #:green-primary (xyY 0.265 0.69 1.)
  #:blue-primary (xyY 0.15 0.06 1.)
  #:trc (2.4 0.948 0.052 0.0774 0.0405))

;; ProPhoto RGB
(define-rgb-colorspace prophoto
  #:reference-white 'D50
  #:red-primary (xyY 0.7347 0.2653 1.)
  #:green-primary (xyY 0.1596 0.8404 1.)
  #:blue-primary (xyY 0.0366 0.0001 1.)
  #:trc (1.8))

;; Adobe RGB (1998)
(define-rgb-colorspace adobe
  #:reference-white 'D65
  #:red-primary (xyY 0.64 0.33 1.)
  #:green-primary (xyY 0.21 0.71 1.)
  #:blue-primary (xyY 0.15 0.06 1.)
  #:trc (2.2))

;; ITU-R BT.2020 Reference Display
(define-rgb-colorspace rec2020
  #:reference-white 'D65
  #:red-primary (xyY 0.708 0.292 1.)
  #:green-primary (xyY 0.170 0.797 1.)
  #:blue-primary (xyY 0.131 0.046 1.)
  #:trc (2.4))

;; Generic rgb->xyz function.
(define (rgb->xyz r)
  (cond
    [(rgb->xyz? r) ((rgb->xyz-ref r) r)]
    [else
     (raise-argument-error 'rgb->xyz "rgb-colorspace with `rgb->xyz' property" r)]))
