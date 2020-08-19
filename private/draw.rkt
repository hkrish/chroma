#lang racket/base

(require pict
         racket/class
         racket/contract
         racket/draw
         racket/list
         "./base-types.rkt"
         "./conversions.rkt"
         "./palette.rkt")

(provide (all-defined-out))


(define color->current-display-color-space
  (make-parameter
   color->rgb/srgb
   (lambda (cp)
     (if (procedure-arity-includes? cp 1)
         cp
         (raise-argument-error 'color->current-display-color-space "procedure of arity 1" cp)))))

(define (->color% c)
  (unless (color? c) (raise-argument-error '->color% "color?" c))
  (let ([c ((color->current-display-color-space) c)])
    (unless (rgb-space? c)
      (raise-argument-error
       '->color%
       "(color->current-display-color-space) should produce a rgb color"
       c))
    (make-color (rgb-space-r c) (rgb-space-g c) (rgb-space-b c))))

(define/contract (draw-swatch c
                              #:size [wh 25]
                              #:corner-radius [cr #f]
                              #:border-color [bc #f]
                              #:border-width [bw #f])
  (->* (color?) (#:size (or/c real? (list/c real? real?))
                 #:corner-radius (or/c #f real?)
                 #:border-color (or/c #f color?)
                 #:border-width (or/c #f real?))
       pict?)
  (let ([wh (if (pair? wh) wh (list wh wh))]
        [db? (or bc bw)]
        [bc (and bc (->color% bc))]
        [ins (if bw (/ bw 2.0) 0.0)])
    (inset
     (if cr
         (filled-rounded-rectangle (car wh) (cadr wh) cr
                                   #:color (->color% c)
                                   #:draw-border? db?
                                   #:border-color bc
                                   #:border-width bw)
         (filled-rectangle (car wh) (cadr wh)
                           #:color (->color% c)
                           #:draw-border? db?
                           #:border-color bc
                           #:border-width bw))
     ins)))

(define/contract (draw-swatch-list cls
                                   #:max-width [tw #f]
                                   #:swatches-per-row [spr #f]
                                   #:swatch-size [wh 25]
                                   #:spacing [spc #f]
                                   #:background [bg #f]
                                   #:corner-radius [cr #f]
                                   #:border-color [bc #f]
                                   #:border-width [bw #f])
  (->* ((listof color?)) (#:max-width (or/c #f (and/c (>/c 0) real?))
                          #:swatches-per-row (or/c #f (integer-in 1 #f))
                          #:swatch-size (or/c real? (list/c real? real?))
                          #:spacing (or/c real? (list/c (or/c #f real?) (or/c #f real?)))
                          #:background (or/c #f color?)
                          #:corner-radius (or/c #f real?)
                          #:border-color (or/c #f color?)
                          #:border-width (or/c #f real?))
       pict?)
  (let*-values
      ([(w h) (if (pair? wh) (values (car wh) (cadr wh)) (values wh wh))]
       [(ncls) (length cls)]
       [(bw_) (if bw (/ bw -2.0) 0)]
       [(spcx spcy) (cond
                      [(pair? spc) (values (car spc) (cadr spc))]
                      [spc (values spc spc)]
                      [else (values bw_ bw_)])]
       [(sw/row/spr) (if spr spr ncls)]
       [(sw/row) (if tw (floor (/ (- tw spcx) (+ spcx w))) sw/row/spr)]
       [(nrows) (ceiling (/ ncls sw/row))]
       [(sws) (map (lambda (c) (draw-swatch c #:size wh
                                       #:corner-radius cr
                                       #:border-color bc
                                       #:border-width bw))
                   cls)])
    (apply vl-append spcy
           (let loop ([sws sws])
             (cond
               [(null? sws) sws]
               [else
                (let-values ([(row left) (if (> (length sws) sw/row)
                                             (split-at sws sw/row)
                                             (values sws '()))])
                  (cons (apply ht-append spcx row) (loop left)))])))))

(define/contract (draw-palette plt width height)
  (-> (procedure-arity-includes/c 1) (integer-in 1 #f) (integer-in 1 #f) pict?)
  (let ([cls (map ->color% (quantize plt width))])
    (dc
     (lambda (dc dx dy)
       (define old-brush (send dc get-brush))
       (define old-pen (send dc get-pen))
       (send dc set-brush "black" 'transparent)
       (for ([x (in-range width)]
             [c (in-list cls)])
         (let ([path (new dc-path%)])
           (send dc set-pen c 1 'solid)
           (send path move-to 0.5 0)
           (send path line-to 0.5 height)
           (send dc draw-path path (+ dx x) dy)))
       (send dc set-brush old-brush)
       (send dc set-pen old-pen))
     width
     height)))

(define/contract (draw-palette-table plt ncols #:swatch-size [wh 25])
  (->* ((procedure-arity-includes/c 1) (integer-in 2 #f)) (#:swatch-size real?) pict?)
  (let* ([ncls (* ncols ncols)]
         [cls (list->vector (quantize plt ncls))])
    (apply ht-append
           (let loop-h ([cols ncols] [colidx 0] [colinc 1])
             (cond
               [(= 0 cols) '()]
               [else
                (cons
                 (apply vl-append
                        (let loop-v ([rows ncols] [rowidx colidx] [rowinc 2])
                          (cond
                            [(= rows 0) '()]
                            [else
                             (let ([c (vector-ref cls rowidx)])
                               (cons (draw-swatch c #:size wh)
                                     (loop-v (sub1 rows) (+ rowidx rowinc) (add1 rowinc))))])))
                 (loop-h (sub1 cols) (+ colidx colinc) (add1 colinc)))])))))
