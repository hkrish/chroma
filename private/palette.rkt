#lang racket/base

(require racket/flonum
         math/flonum
         racket/match
         racket/math
         racket/contract
         "./helpers.rkt"
         "./base-types.rkt"
         "./rgb.rkt"
         "./lch.rkt"
         "./luv.rkt"
         "./conversions.rkt")

(provide cube-helix quantize)

;; palette : (float -> color?)

(define/contract (quantize palette n)
  (-> (procedure-arity-includes/c 1) (integer-in 2 #f) (listof any/c))
  (let ([n-1 (fl- (fl n) 1.0)])
    (for/list ([i (in-range n)])
      (palette (fl/ (fl i) n-1)))))

;; `Cubehelix' palette
;;
;; Reference: Green, D.A.. (2011). "A colour scheme for the display of astronomical
;;            intensity images." Bulletin of The Astronomical Society of India. 39.
;; Source: http://astron-soc.in/bulletin/11June/289392011.pdf

;; <commentary from the original FORTRAN program>
;; The input parameters controlling the colour helix are:
;; - START colour (1=red, 2=green, 3=blue; e.g. 0.5=purple);
;; - ROTS  rotations in colour (typically -1.5 to 1.5, e.g. -1.0
;;         is one blue->green->red cycle);
;; - HUE   for hue intensity scaling (in the range 0.0 (B+W) to 1.0
;;         to be strictly correct, larger values may be OK with particular start/end
;;         colours);
;; - GAMMA set the gamma correction for intensity.

(define/contract (cube-helix [count 'continuous]
                             #:start-hue [start 270.0]
                             #:rotation [rot 1.0]
                             #:chroma [chroma 1.0]
                             #:gamma [gamma 1.0])
  (->* () ((or/c 'continuous (integer-in 2 #f))
           #:start-hue real?
           #:rotation (between/c -2 2)
           #:chroma (>=/c 0.0)
           #:gamma (>=/c 0.0))
       (or/c procedure? list?))
  (let* ([2pi (fl* 2.0 pi)]
         [start (fl+ (fl/ (degrees->radians (flmodulo start 360.0)) 2pi) 1.0)]
         [rot (fl rot)]
         [chroma (fl chroma)]
         [gamma (fl gamma)])
    (define palette
      (lambda (t)
        (let* ([angle (fl* 2pi (fl+ start (fl* rot t)))]
               [cosa (flcos angle)]
               [sina (flsin angle)]
               [t (fl t)]
               [t (flexpt t gamma)]
               [amp (fl* chroma t (fl/ (fl- 1.0 t) 2.0))])
          (rgb/srgb
           (unnorm/clamp (fl+ t (fl* amp (fl+ (fl* -0.14861 cosa) (fl* 1.78277 sina)))))
           (unnorm/clamp (fl+ t (fl* amp (fl+ (fl* -0.29227 cosa) (fl* -0.90649 sina)))))
           (unnorm/clamp (fl+ t (fl* amp 1.97294 cosa)))))))
    (if (eq? count 'continuous)
        palette
        (quantize palette count))))
