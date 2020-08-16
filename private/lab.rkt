#lang racket/base

(require "./base-types.rkt"
         "./helpers.rkt"
         racket/unsafe/ops
         math/flonum)

(provide (all-defined-out))

(struct lab color (l a b)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc color-printer)])

(define* (xyz->lab (xyz x y z))
  (let* ([Xr (unsafe-struct-ref illuminant/pcs 0)]
         [Yr (unsafe-struct-ref illuminant/pcs 1)]
         [Zr (unsafe-struct-ref illuminant/pcs 2)]
         [xr (fl/ (fl x) Xr)]
         [yr (fl/ (fl y) Yr)]
         [zr (fl/ (fl z) Zr)]
         [e (fl/ 216. 24389.)]
         [k (fl/ 24389. 27.)]
         [fx (if (fl> xr e)
                 (flexpt xr (fl/ 1. 3.))
                 (fl/ (fl+ (fl* k xr) 16.) 116.))]
         [fy (if (fl> yr e)
                 (flexpt yr (fl/ 1. 3.))
                 (fl/ (fl+ (fl* k yr) 16.) 116.))]
         [fz (if (fl> zr e)
                 (flexpt zr (fl/ 1. 3.))
                 (fl/ (fl+ (fl* k zr) 16.) 116.))])
    (lab (fl- (fl* 116. fy) 16.)
         (fl* 500. (fl- fx fy))
         (fl* 200. (fl- fy fz)))))

(define* (lab->xyz (lab l a b))
  (let* ([Xr (unsafe-struct-ref illuminant/pcs 0)]
         [Yr (unsafe-struct-ref illuminant/pcs 1)]
         [Zr (unsafe-struct-ref illuminant/pcs 2)]
         [e (fl/ 216. 24389.)]
         [k (fl/ 24389. 27.)]
         [l (fl l)]
         [fy (fl/ (fl+ l 16.) 116.)]
         [fx (fl+ (fl/ (fl a) 500.) fy)]
         [fz (fl- fy (fl/ (fl b) 200.))]
         [fx3 (flexpt fx 3.0)]
         [fz3 (flexpt fz 3.0)]
         [xr (if (fl> fx3 e)
                 fx3
                 (fl/ (fl- (fl* 116. fx) 16.) k))]
         [yr (if (fl> l (fl* k e))
                 (flexpt fy 3.0)
                 (fl/ l k))]
         [zr (if (fl> fz3 e)
                 fz3
                 (fl/ (fl- (fl* 116. fz) 16.) k))])
    (xyz (fl* xr Xr) (fl* yr Yr) (fl* zr Zr))))
