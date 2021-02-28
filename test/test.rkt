#lang racket/base

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/match
           math/flonum
           "../private/helpers.rkt"
           "../private/base-types.rkt"
           "../private/rgb.rkt"
           "../private/luv.rkt"
           "../private/lab.rkt"
           "../private/lch.rkt"
           "../private/conversions.rkt"
           "../private/difference.rkt")

  (require "./data/brewer-rgbs.rkt"
           "./data/display-p3.rkt"
           "./data/adobe98.rkt"
           "./data/rec2020.rkt"
           "./data/prophoto.rkt"
           "./data/xyz.rkt"
           "./data/luv.rkt"
           "./data/lab.rkt"
           "./data/lch-ab.rkt"
           "./data/ciede2000.rkt")

  (define (color->list c)
    (match c
      [(rgb-space r g b) (list r g b)]
      [(xyz a b c) (list a b c)]
      [(luv a b c) (list a b c)]
      [(lab a b c) (list a b c)]
      [(lch a b c) (list a b c)]))

  ;; We can tolerate a difference of `2'. Can happen due to rounding etc.
  (define* (rgb-list=? (list a b c) (list p q r))
    (and
     (<= (abs (- a p)) 2)
     (<= (abs (- b q)) 2)
     (<= (abs (- c r)) 2)))

  (define* (xyz-list=? (list a b c) (list p q r))
    (let ([k 1e-3])
      (and
       (<= (flabs (fl- (fl a) (fl p))) k)
       (<= (flabs (fl- (fl b) (fl q))) k)
       (<= (flabs (fl- (fl c) (fl r))) k))))

  (define* (l*-list=? (list a b c) (list p q r))
    (let ([k 1e-1])
      (and
       (<= (flabs (fl- (fl a) (fl p))) k)
       (<= (flabs (fl- (fl b) (fl q))) k)
       (<= (flabs (fl- (fl c) (fl r))) k))))

  (define* (lch-list=? (list a b c) (list p q r))
    (let ([k 1e-1])
      (and
       (<= (flabs (fl- (fl a) (fl p))) k)
       (<= (flabs (fl- (fl b) (fl q))) k)
       (if (<= (flabs (fl b)) 1e-7)
           #t                             ; If chroma ~= 0.0, hue doesn't matter
           (<= (flabs (fl- (fl c) (fl r))) k)))))

  (define-binary-check (check-rgb=?/list rgb-list=? actual expected))
  (define-binary-check (check-xyz=?/list xyz-list=? actual expected))
  (define-binary-check (check-l*=?/list l*-list=? actual expected))
  (define-binary-check (check-lch=?/list lch-list=? actual expected))

  (run-tests
   (test-suite
    "sRGB -> Display-P3"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> Display-P3"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->display-p3)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->rgb/display-p3 csrc)])
        (test-case (format "~a , ~a" src dst)
          (check-rgb=?/list (color->list ctgt) dst))))))

  (run-tests
   (test-suite
    "sRGB -> Adobe98"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> Adobe98"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->adobe98)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->rgb/adobe csrc)])
        (test-case (format "~a , ~a" src dst)
          (check-rgb=?/list (color->list ctgt) dst))))))

  (run-tests
   (test-suite
    "sRGB -> rec2020"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> rec2020"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->rec2020)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->rgb/rec2020 csrc)])
        (test-case (format "~a , ~a" src dst)
          (check-rgb=?/list (color->list ctgt) dst))))))

  (run-tests
   (test-suite
    "sRGB -> ProPhoto"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> ProPhoto"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->prophoto)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->rgb/prophoto csrc)])
        (test-case (format "~a , ~a" src dst)
          (check-rgb=?/list (color->list ctgt) dst))))))

  (run-tests
   (test-suite
    "sRGB -> XYZ -> sRGB"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> XYZ -> sRGB"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->xyz)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->xyz csrc)]
             [cback (color->rgb/srgb ctgt)])
        (test-case (format "~a , ~a" src dst)
          (check-xyz=?/list (color->list ctgt) dst))
        (test-case (format "~a , ~a" dst src)
          (check-rgb=?/list (color->list cback) src))))))

  (run-tests
   (test-suite
    "sRGB -> Luv -> sRGB"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> Luv -> sRGB"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->luv)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->luv csrc)]
             [cback (color->rgb/srgb ctgt)])
        (test-case (format "~a , ~a" src dst)
          (check-l*=?/list (color->list ctgt) dst))
        (test-case (format "~a , ~a" dst src)
          (check-rgb=?/list (color->list cback) src))))))

  (run-tests
   (test-suite
    "sRGB -> Lab -> sRGB"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> Lab -> sRGB"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->lab)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->lab csrc)]
             [cback (color->rgb/srgb ctgt)])
        (test-case (format "~a , ~a" src dst)
          (check-l*=?/list (color->list ctgt) dst))
        (test-case (format "~a , ~a" dst src)
          (check-rgb=?/list (color->list cback) src))))))

  (run-tests
   (test-suite
    "sRGB -> LCHab -> sRGB"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: sRGB -> LCHab -> sRGB"))
    (for ([src (in-list test-data/brewer)]
          [dst (in-list test-data/srgb->lch/ab)])
      (let* ([csrc (apply rgb/srgb src)]
             [ctgt (color->lch/ab csrc)]
             [cback (color->rgb/srgb ctgt)])
        (test-case (format "~a , ~a" src dst)
          (check-lch=?/list (color->list ctgt) dst))
        (test-case (format "~a , ~a" dst src)
          (check-rgb=?/list (color->list cback) src))))))

  (run-tests
   (test-suite
    "color-difference/ciede2000"
    #:before (lambda ()
               (newline)
               (displayln "[test-suite]: color-difference/ciede2000"))
    (for ([src (in-list test-data/difference/ciede2000)])
      (let* ([c1 (apply rgb/srgb (car src))]
             [c2 (apply rgb/srgb (cadr src))]
             [adiff (caddr src)]
             [diff (color-difference/ciede2000 c1 c2)])
        (test-case (format "~a , ~a" (car src) (cadr src))
          (check-= diff adiff 0.1))))))
  )
