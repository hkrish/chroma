#lang racket/base

(require "./base-types.rkt"
         "./helpers.rkt"
         racket/unsafe/ops
         math/flonum)

(provide (all-defined-out))

(struct luv color (l u v)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc color-printer)])

(define* (xyz->luv (xyz x y z))
  (let* ([Xr (unsafe-struct-ref illuminant/pcs 0)]
         [Yr (unsafe-struct-ref illuminant/pcs 1)]
         [Zr (unsafe-struct-ref illuminant/pcs 2)]
         [x (fl x)]
         [y (fl y)]
         [yr (fl/ y Yr)]
         [d* (fl+ x (fl* 15. (fl y)) (fl* 3. (fl z)))]
         [u* (fl/ (fl* 4. x) d*)]
         [v* (fl/ (fl* 9. y) d*)]
         [dr* (fl+ Xr (fl* 15. Yr) (fl* 3. Zr))]
         [ur* (fl/ (fl* 4. Xr) dr*)]
         [vr* (fl/ (fl* 9. Yr) dr*)]
         [e (fl/ 216. 24389.)]
         [k (fl/ 24389. 27.)]
         [l (if (fl> yr e)
                (fl- (fl* 116. (flexpt yr (fl/ 1. 3.))) 16.)
                (fl* k yr))]
         [u (fl* 13. l (fl- u* ur*))]
         [v (fl* 13. l (fl- v* vr*))])
    (luv l u v)))

(define* (luv->xyz (luv l u v))
  (let* ([Xr (unsafe-struct-ref illuminant/pcs 0)]
         [Yr (unsafe-struct-ref illuminant/pcs 1)]
         [Zr (unsafe-struct-ref illuminant/pcs 2)]
         [e (fl/ 216. 24389.)]
         [k (fl/ 24389. 27.)]
         [y (if (fl> l (fl* k e))
                (flexpt (fl/ (fl+ l 16.) 116.) 3.)
                (fl/ l k))]
         [d0 (fl+ Xr (fl* 15. Yr) (fl* 3. Zr))]
         [u0 (fl/ (fl* 4. Xr) d0)]
         [v0 (fl/ (fl* 9. Yr) d0)]
         [d (fl* y (fl- (fl/ (fl* 39. l) (fl+ v (fl* 13. l v0))) 5.))]
         [b (fl* -5. y)]
         [a (fl* (fl/ 1. 3.) (fl- (fl/ (fl* 52. l) (fl+ u (fl* 13. l u0))) 1.))]
         [x (fl/ (fl- d b) (fl- a (fl/ -1. 3.)))]
         [z (fl+ (fl* x a) b)])
    (xyz x y z)))