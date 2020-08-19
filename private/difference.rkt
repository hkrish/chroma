#lang racket/base

(require racket/flonum
         math/flonum
         racket/match
         racket/math
         "./base-types.rkt"
         "./lab.rkt"
         (only-in "./conversions.rkt" color->lab))

(provide color-difference/euclidian
         color-difference/ciede2000
         (rename-out [color-difference/ciede2000 color-difference]))


(define (color-difference/euclidian c1 c2)
  (match* ((color->xyz c1) (color->xyz c2))
    [((xyz x1 y1 z1) (xyz x2 y2 z2))
     (let ([d1 (fl- x1 x2)]
           [d2 (fl- y1 y2)]
           [d3 (fl- z1 z2)])
       (flsqrt (fl+ (fl* d1 d1) (fl* d2 d2) (fl* d3 d3))))]))


(define-syntax-rule (flsqr a) (let ([x a]) (fl* x x)))

;; Calculates difference between two colors according to the CIEDE2000 function.
;;  Refrence: http://www.brucelindbloom.com/
(define (color-difference/ciede2000 c1 c2)
  (match* ((color->lab c1) (color->lab c2))
    [((lab l1 a1 b1) (lab l2 a2 b2))
     (let* ([kL 1.0]
            [kC 1.0]
            [kH 1.0]
            [L_^ (fl/ (fl+ l1 l2) 2.0)]
            [c1 (flsqrt (fl+ (fl* a1 a1) (fl* b1 b1)))]
            [c2 (flsqrt (fl+ (fl* a2 a2) (fl* b2 b2)))]
            [c_ (fl/ (fl+ c1 c2) 2.0)]
            [c_7 (flexpt c_ 7.0)]
            [g (fl/ (fl- 1.0 (flsqrt (fl/ c_7 (fl+ c_7 6103515625.0)))) 2.0)] ; 6103515625 = 25^7
            [a1^ (fl* a1 (fl+ 1.0 g))]
            [a2^ (fl* a2 (fl+ 1.0 g))]
            [c1^ (flsqrt (fl+ (fl* a1^ a1^) (fl* b1 b1)))]
            [c2^ (flsqrt (fl+ (fl* a2^ a2^) (fl* b2 b2)))]
            [c_^ (fl/ (fl+ c1^ c2^) 2.0)]
            [h1^ (radians->degrees (atan b1 a1^))]
            (h1^ (if (fl< h1^ 0.0) (fl+ h1^ 360.0) h1^))
            [h2^ (radians->degrees (atan b2 a2^))]
            (h2^ (if (fl< h2^ 0.0) (fl+ h2^ 360.0) h2^))
            [h_^ (if (fl> (flabs (fl- h1^ h2^)) 180.0)
                     (fl/ (fl+ h1^ h2^ 360.0) 2.0)
                     (fl/ (fl+ h1^ h2^) 2.0))]
            [t (fl+ (fl- 1.0 (fl* 0.17 (flcos (degrees->radians (fl- h_^ 30.0)))))
                    (fl* 0.24 (flcos (degrees->radians (fl* 2.0 h_^))))
                    (fl* 0.32 (flcos (degrees->radians (fl+ (fl* 3.0 h_^) 6.0))))
                    (fl* -0.20 (flcos (degrees->radians (fl- (fl* 4.0 h_^) 63.0)))))]
            [dh^ (if (fl<= (flabs (fl- h2^ h1^)) 180.0)
                     (fl- h2^ h1^)
                     (if (fl<= h2^ h1^)
                         (fl+ (fl- h2^ h1^) 360.0)
                         (fl- h2^ h1^ 360.0)))]
            [dL^ (fl- l2 l1)]
            [dC^ (fl- c2^ c1^)]
            [dH^ (fl* 2.0 (flsqrt (fl* c1^ c2^)) (flsin (degrees->radians (fl/ dh^ 2.0))))]
            [sL (fl+ 1.0 (fl/ (fl* 0.015 (flsqr (fl- L_^ 50.0)))
                              (flsqrt (+ 20.0 (flsqr (- L_^ 50.0))))))]
            [sC (fl+ 1.0 (fl* 0.045 c_^))]
            [sH (fl+ 1.0 (fl* 0.015 c_^ t))]
            [dTheta (fl* 30.0 (flexp (fl- (flsqr (fl/ (fl- h_^ 275.0) 25.0)))))]
            [c_^7 (flexpt c_^ 7.0)]
            [rT (fl* -2.0 (flsqrt (fl/ c_^7 (fl+ c_^7 6103515625.0)))
                     (flsin (degrees->radians (fl* 2.0 dTheta))))])
       (flsqrt (fl+ (flsqr (fl/ dL^ (fl* kL sL)))
                    (flsqr (fl/ dC^ (fl* kC sC)))
                    (flsqr (fl/ dH^ (fl* kH sH)))
                    (fl* rT (fl/ dC^ (fl* kC sC)) (fl/ dH^ (fl* kH sH))))))]))
