#lang racket/base

(require (for-syntax racket/base)
         math/flonum
         racket/match
         racket/math
         racket/contract
         "./helpers.rkt"
         "./base-types.rkt"
         "./rgb.rkt"
         "./lch.rkt"
         "./conversions.rkt"
         "./matrix3.rkt"
         "./difference.rkt"
         "./geometry/bezier-quad.rkt")

(provide (struct-out palette)
         palette-quantize
         palette-reverse
         palette-select-range
         make-palette/cube-helix
         make-palette/sequential
         make-palette/diverging
         make-palette/qualitative)


;; FIX: TODO: This should be set in the main module
(current-display-color-space rgb/srgb)

(struct palette (f type)
  #:property prop:procedure (struct-field-index f))

(define/contract (palette-quantize pal n #:minimum-distance [mindist 50])
  (->* (palette? (integer-in 2 #f))
       (#:minimum-distance (>/c 0))
       (listof any/c))
  (let ([f (palette-f pal)])
    (cond
      [(eq? (palette-type pal) 'qualitative)
       (define (dist* c ls)
         (for/fold ([mn +inf.0])
                   ([p (in-list ls)])
           (let ([d (color-difference c p)])
             (if (< d mn) d mn))))
       (let loop ([count n] [cls '()])
         (cond
           [(<= count 0) cls]
           [else
            (let loop/find ([attempts 50] [dbest -inf.0] [cbest #f])
              (cond
                [(<= attempts 0)
                 (loop (sub1 count) (cons cbest cls))]
                [else
                 (define cnext (f (random)))
                 (define d (dist* cnext cls))
                 (if (< d mindist)
                     (let-values ([(dm cm) (if (>= d dbest)
                                               (values d cnext)
                                               (values dbest cbest))])
                       (loop/find (sub1 attempts) dm cm))
                     (loop (sub1 count) (cons cnext cls)))]))]))]
      [else
       (let ([n-1 (fl- (fl n) 1.0)])
         (for/list ([i (in-range n)])
           (f (fl/ (fl i) n-1))))])))

(define/contract (palette-reverse pal)
  (-> palette? palette?)
  (let ([f (palette-f pal)])
    (palette (lambda (t) (f (- 1.0 t))) (palette-type pal))))

(define/contract (palette-select-range pal t0 t1)
  (-> palette? (between/c 0 1) (between/c 0 1) palette?)
  (let ([f (palette-f pal)])
    (palette
     (lambda (t)
       (let ([td (t1 - t0)])
         (f (+ t0 (* td t)))))
     (palette-type pal))))


;; ------------------------------------------------------------
;; `Cubehelix' palette
;;
;; Reference: Green, D.A.. (2011). "A colour scheme for the display of astronomical
;;            intensity images." Bulletin of The Astronomical Society of India. 39.
;; Source: http://astron-soc.in/bulletin/11June/289392011.pdf

(define/contract (make-palette/cube-helix [count 'continuous]
                                          #:start-hue [start 270.0]
                                          #:rotation [rot 1.0]
                                          #:chroma [chroma 1.0]
                                          #:gamma [gamma 1.0])
  (->* () ((or/c 'continuous (integer-in 2 #f))
           #:start-hue real?
           #:rotation (between/c -2 2)
           #:chroma (>=/c 0.0)
           #:gamma (>=/c 0.0))
       (or/c palette? list?))
  (let* ([2pi (fl* 2.0 pi)]
         [start (fl+ (fl/ (degrees->radians (flmodulo start 360.0)) 2pi) 1.0)]
         [rot (fl rot)]
         [chroma (fl chroma)]
         [gamma (fl gamma)])
    (define pal
      (palette
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
            (unnorm/clamp (fl+ t (fl* amp 1.97294 cosa))))))
       'cube-helix))
    (if (eq? count 'continuous)
        pal
        (palette-quantize pal count))))


;;------------------------------------------------------------
;; Palette generation with intuitive parameters
;;
;;Reference: http://alexandria.tue.nl/extra2/afstversl/wsk-i/wijffelaars2008.pdf

;; lch arithmetic
(define-syntax (define-lch-point-op stx)
  (syntax-case stx ()
    [(_ (id c1 c2) op ->res)
     #'(define (id c1 c2)
         (match*/derived
          (c1 c2) (id c1 c2)
          [((lch-point a b c) (lch-point p q r)) (->res (op a p) (op b q) (op c r))]
          [((lch-point a b c) r) (->res (op a r) (op b r) (op c r))]
          [(r (lch a b c)) (->res (op r a) (op r b) (op r c))]))]))

(define-lch-point-op (lch-point+ c1 c2) fl+ lch)
(define-lch-point-op (lch-point- c1 c2) fl- lch)
(define-lch-point-op (lch-point* c1 c2) fl* lch)

(define* (lch-point-lerp (lch-point a b c) (lch-point p q r) s)
  (let ([t (- 1.0 s)])
    (lch-point (fl+ (fl* t a) (fl* s p))
               (fl+ (fl* t b) (fl* s q))
               (fl+ (fl* t c) (fl* s r)))))

(define* (color-distance/linear (lch-point l1 _ _) (lch-point l2 _ _))
  (flabs (fl- l2 l1)))

(define* (color-distance/log (lch-point l1 _ _) (lch-point l2 _ _))
  (flabs (fllog (fl/ (fl- 125. l2) (fl- 125. l1)))))

(define color-distance/cie color-difference/ciede2000)

(struct lch-point lch ()
  #:transparent
  #:methods gen:point
  [(define point+ lch-point+)
   (define point- lch-point-)
   (define point* lch-point*)
   (define point-lerp lch-point-lerp)])

(define* (lch->lch-point (lch l c h))
  (lch-point l c h))

(define* (lch-point->lch (lch-point l c h))
  (lch l c h))

(define (mix-hue a h0 h1)
  (let ([m (fl- (flremainder (fl+ 180.0 h1 (fl- h0)) 360.0) 180.0)])
    (flmodulo (fl+ h0 (fl* a m)) 360.0)))

(define (hue-difference h0 h1)
  (let* ([h0 (flmodulo h0 360.0)]
         [h1 (flmodulo h1 360.0)]
         [hd (flabs (fl- h1 h0))])
    (if (fl< hd 180.0) hd (fl- 360.0 hd))))

(define (clamp-chroma lch*)
  (define (max-chroma l h)
    (let*-values ([(p-mid) (color->lch (saturate-hue h))]
                  [(p-mid-l p-mid-c) (values (lch-l p-mid) (lch-c p-mid))]
                  [(p-end-l) (if (fl<= l p-mid-l) 0.0 100.0)]
                  [(a) (fl/ (fl- p-end-l l) (fl- p-end-l p-mid-l))])
      (fl* a p-mid-c)))
  (match lch*
    [(lch-point l c h) (lch-point l (flmin c (max-chroma l h)) h)]
    [(lch l c h) (lch l (flmin c (max-chroma l h)) h)]))

;; Given Hue angle (degrees) returns to the most saturated RGB color (in
;; current-display-color-space) corresponding to that hue.
;;  Reference: http://alexandria.tue.nl/extra2/afstversl/wsk-i/wijffelaars2008.pdf
;;             (modified to handle different RGB display color-spaces)
(define (saturate-hue clr)
  (define hue
    (flmodulo
     (cond
       [(color? clr) (lch-h (color->lch clr))]
       [(real? clr) clr ]
       [else (raise-argument-error 'saturate-hue "(or/c color? real?)" clr)])
     360.0))
  (define make-display-rgb (car (current-display-color-space)))
  (define a-display-color (make-display-rgb 0 0 0))
  (define rgb->xyz/mat (rgb->xyz:matrix-ref a-display-color))
  (define inv-trc-fn (cdr (trc-procedures-ref a-display-color)))
  (let*-values
      ([(hr) (lch-h (color->lch (make-display-rgb 255 0 0)))]   ; hue angle for red
       [(hy) (lch-h (color->lch (make-display-rgb 255 255 0)))] ; hue angle for yellow
       [(hg) (lch-h (color->lch (make-display-rgb 0 255 0)))]   ; hue angle for green
       [(hc) (lch-h (color->lch (make-display-rgb 0 255 255)))] ; hue angle for cyan
       [(hb) (lch-h (color->lch (make-display-rgb 0 0 255)))]   ; hue angle for blue
       [(hp) (lch-h (color->lch (make-display-rgb 255 0 255)))] ; hue angle for purple
       [(edge vari maxi)
        (cond
          [(and (<= hr hue) (< hue hy)) (values 'ry 1 0)]
          [(and (<= hy hue) (< hue hg)) (values 'yg 0 1)]
          [(and (<= hg hue) (< hue hc)) (values 'gc 2 1)]
          [(and (<= hc hue) (< hue hb)) (values 'cb 1 2)]
          [(and (<= hb hue) (< hue hp)) (values 'bp 0 2)]
          [else (values 'pr 2 0)])]
       [(mrX mrY mrZ) (3x3-col-ref rgb->xyz/mat vari)]
       [(mtX mtY mtZ) (3x3-col-ref rgb->xyz/mat maxi)]
       [(huer) (degrees->radians hue)]
       [(a) (fl- (flsin huer))]
       [(b) (flcos huer)]
       [(Xr) (fl (xyz-x illuminant/pcs))]
       [(Yr) (fl (xyz-y illuminant/pcs))]
       [(Zr) (fl (xyz-z illuminant/pcs))]
       [(dr*) (fl+ Xr (fl* 15.0 Yr) (fl* 3.0 Zr))]
       [(dt) (fl+ mtX (fl* 15.0 mtY) (fl* 3.0 mtZ))]
       [(nt) (fl+ (fl* 4.0 a mtX) (fl* 9.0 b mtY))]
       [(dr) (fl+ mrX (fl* 15.0 mrY) (fl* 3.0 mrZ))]
       [(nr) (fl+ (fl* 4.0 a mrX) (fl* 9.0 b mrY))]
       [(un vn) (values (fl/ (fl* 4.0 Xr) dr*)
                        (fl/ (fl* 9.0 Yr) dr*))]
       [(aubv) (fl+ (fl* a un) (fl* b vn))]
       [(crp) (fl/ (fl- (fl* aubv dt) nt) (fl- nr (fl* aubv dr)))]
       [(crp-gc _1 _2) (inv-trc-fn crp 0.0 0.0)])
    (cond
      [(eq? edge 'ry) (make-display-rgb 255 crp-gc 0)]
      [(eq? edge 'yg) (make-display-rgb crp-gc 255 0)]
      [(eq? edge 'gc) (make-display-rgb 0 255 crp-gc)]
      [(eq? edge 'cb) (make-display-rgb 0 crp-gc 255)]
      [(eq? edge 'bp) (make-display-rgb crp-gc 0 255)]
      [else (make-display-rgb 255 0 crp-gc)])))

(define/contract (make-palette/sequential [count 'continuous]
                                          #:hue [h 0]
                                          #:brightness [b 0.75]
                                          #:saturation [s 0.6]
                                          #:contrast [con #f]
                                          #:hue-shift [hs 0.15]
                                          #:scale [scale 'log])
  (->* () ((or/c 'continuous (integer-in 2 #f))
           #:hue real?
           #:brightness (between/c 0 1)
           #:saturation (between/c 0 1)
           #:contrast (or/c (between/c 0 1) #f)
           #:hue-shift (or/c #f
                             (between/c 0 1)
                             (cons/c (between/c 0 1) color?))
           #:scale (or/c 'linear 'log 'cie))
       (or/c palette? (listof color?)))
  ;; Hue-shift amount and target color
  (define-values (w cb)
    (cond
      [(pair? hs)
       (values (fl (car hs)) (color->lch (cdr hs)))]
      [(real? hs)
       (let ([make-display-rgb (car (current-display-color-space))])
         ;; By default shift towards yellow. Works for a wide variety of hues.
         (values (fl hs) (color->lch (make-display-rgb 255.0 255.0 0.0))))]
      [else (values #f #f)]))
  ;; Distance function to parameterize LCH bezier curves
  (define distancef
    (cond
      [(eq? 'linear scale) color-distance/linear]
      [(eq? 'log scale) color-distance/log]
      [(eq? 'cie scale) color-distance/cie]))
  ;; Default contrast value is selected based on number of output colors
  (define c
    (cond
      [con (fl con)]
      [(eq? count 'continuous) 1.0]
      [else (flmin 0.88 (fl+ 0.34 (fl* 0.06 (fl count))))]))
  (let* ([s (fl s)]
         [p1 (lch->lch-point (color->lch (saturate-hue h)))]
         [h (lch-h p1)]
         [p0 (lch-point 0.0 0.0 h)]
         [p2 (cond
               [(and w (fl> w 0.0))
                (clamp-chroma
                 (lch-point (fl+ (fl* (fl- 1.0 w) 100.0) (fl* w (lch-l cb)))
                            (fl* w s (lch-c cb))
                            (mix-hue w h (lch-h cb))))]
               [else (lch-point 100.0 0.0 h)])]
         [q0 (lch-point-lerp p0 p1 s)]
         [q2 (lch-point-lerp p2 p1 s)]
         [q1 (lch-point-lerp q0 q2 0.5)]
         [B0 (bezier-quad 2 (list p0 q0 q1))]
         [B1 (bezier-quad 2 (list q1 q2 p2))]
         [curve-pair (cons B0 B1)]
         [curve-pair-point
          (lambda (cs t)
            (let ([t (clamp (fl t))])
              (if (<= t 0.5)
                  (bezier-quad-point (car cs) (fl* 2.0 t))
                  (bezier-quad-point (cdr cs) (fl* 2.0 (fl- t 0.5))))))]
         [length->parameter             ; normalized arc-length (0, 1) to parameter
          (curve-inverse-parameterization curve-pair curve-pair-point distancef)]
         [t0 (fl* (fl- 1.0 c) (fl b))]
         [->length (lambda (t) (fl+ t0 (fl* c (clamp (fl t)))))])
    (define pal
      (palette
       (lambda (t)
         (lch-point->lch (curve-pair-point
                          curve-pair
                          (length->parameter (->length t)))))
       'sequential))
    (if (eq? count 'continuous)
        pal
        (palette-quantize pal count))))

(define/contract (make-palette/diverging [count 'continuous]
                                         #:hue [h '(0 . 120)]
                                         #:midpoint [m 0.5]
                                         #:brightness [b 0.75]
                                         #:saturation [s 0.6]
                                         #:contrast [con #f]
                                         #:hue-shift [hs 0.0]
                                         #:scale [scale 'log])
  (->* () ((or/c 'continuous (integer-in 2 #f))
           #:hue (cons/c real? real?)
           #:midpoint (between/c 0 1)
           #:brightness (between/c 0 1)
           #:saturation (between/c 0 1)
           #:contrast (or/c (between/c 0 1) #f)
           #:hue-shift (or/c #f
                             (between/c 0 1)
                             (cons/c (between/c 0 1) color?))
           #:scale (or/c 'linear 'log 'cie))
       (or/c palette? (listof color?)))
  ;; Default contrast value is selected based on number of output colors.
  (define c
    (cond
      [con (fl con)]
      [(eq? count 'continuous) 1.0]
      [else
       (let ([count* (fl (+ (floor (/ count 2)) 1))])
         (flmin 0.88 (fl- 1.0 (fl* 0.06 (fl- 11.0 count*)))))]))
  (define pal0 (make-palette/sequential 'continuous
                                        #:hue (car h)
                                        #:brightness b
                                        #:saturation s
                                        #:contrast c
                                        #:hue-shift hs
                                        #:scale scale))
  (define pal1 (make-palette/sequential 'continuous
                                        #:hue (cdr h)
                                        #:brightness b
                                        #:saturation s
                                        #:contrast c
                                        #:hue-shift hs
                                        #:scale scale))
  (define make-display-rgb (car (current-display-color-space)))
  (define-values (w n-h)
    (cond
      [(pair? hs) (values (fl (car hs)) (lch-h (color->lch (cdr hs))))]
      [else (values (if (real? hs) (fl hs) 0.0)
                    (lch-h (color->lch (make-display-rgb 255.0 255.0 0.0))))]))
  ;; Neutral color
  (define cn
    (let ([c0 (pal0 1.0)]
          [c1 (pal1 1.0)])
      (clamp-chroma
       (lch (fl/ (fl+ (lch-l c0) (lch-l c1)) 2.0)
            (fl* w (fl/ (fl+ (lch-c c0) (lch-c c1)) 2.0))
            n-h))))
  (let* ([m (fl m)]
         [2e (flabs (fl* 2.0 (fl- (fl m) 0.5)))]
         [t* (cond
               [(fl>= m 0.5) (lambda (t) (fl/ t (fl+ 1.0 2e)))]
               [else (lambda (t) (fl/ (fl+ t 2e) (fl+ 1.0 2e)))])])
    (define pal
      (palette
       (lambda (t)
         (let ([t_ (t* (clamp (fl t)))])
           (cond
             [(fl< t_ 0.5) (pal0 (fl* 2.0 t_))]
             [(fl> t_ 0.5) (pal1 (fl* 2.0 (fl- 1.0 t_)))]
             [else cn])))
       'diverging))
    (if (eq? count 'continuous)
        pal
        (palette-quantize pal count))))

(define/contract (make-palette/qualitative [count 'continuous]
                                           #:hue-range [r 1.0]
                                           #:hue-shift [e 0.0]
                                           #:brightness [b 1.0]
                                           #:saturation [s 0.5]
                                           #:contrast [c 0.5]
                                           #:minimum-distance [mindist 50])
  (->* ()
       ((or/c 'continuous (integer-in 2 #f))
        #:hue-range (between/c 0 1)
        #:hue-shift (between/c 0 1)
        #:brightness (between/c 0 1)
        #:saturation (between/c 0 1)
        #:contrast (between/c 0 1)
        #:minimum-distance (>/c 0))
       (or/c palette? (listof color?)))
  (define make-display-rgb (car (current-display-color-space)))
  ;; In RGB models, yellow typically has maximum lightness of all fully saturated hues in
  ;; CIELuv space (true for all the rgb-spaces defined in `chroma')
  (define yellow (color->lch (make-display-rgb 255 255 0)))
  ;; In RGB models, red typically has maximum chroma in CIELuv space (true for all the
  ;; rgb-spaces defined in `chroma')
  (define c-max (lch-c (color->lch (make-display-rgb 255 0 0))))
  (let* ([e (fl e)]
         [r (fl r)]
         [l0 (fl* (fl b) (lch-l yellow))]
         [l1 (fl* (fl- 1.0 (fl c)) l0)]
         [yellow-h (lch-h yellow)])
    (define pal
      (palette
       (lambda (t)
         (let* ([t (clamp (fl t))]
                [h (flmodulo (fl* 360.0 (fl+ e (fl* r t))) 360.0)]
                [a (fl/ (hue-difference h yellow-h) 180.0)]
                [l (fl+ (fl* (fl- 1.0 a) l0) (fl* a l1))]
                [c-limit (* s c-max)])
           (clamp-chroma (lch l c-limit h))))
       'qualitative))
    (if (eq? count 'continuous)
        pal
        (palette-quantize pal count #:minimum-distance mindist))))
