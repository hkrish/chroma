#lang racket/base

(require racket/generic
         "../helpers.rkt")

(provide gen:point curve-parameterization-samples
         (struct-out bezier-quad)
         bezier-quad-reverse
         bezier-quad-point
         bezier-quad-tangent
         bezier-quad-parameterization
         bezier-quad-inverse-parameterization
         curve-parameterization
         curve-inverse-parameterization)

(define-generics point
  (point+ point point2)
  (point- point point2)
  (point* point point2)
  (point-lerp point point2 t))

(define curve-parameterization-samples (make-parameter 51))

(struct bezier-quad (degree points) #:authentic)

(define* (bezier-quad-reverse (bezier-quad d ps))
  (bezier-quad d (reverse ps)))

(define (bezier-quad-point c t)
  (let loop ([ps (bezier-quad-points c)])
    (cond
      [(null? (cdr ps)) (car ps)]
      [else
       (loop (zipwith (lambda (p1 p2) (point-lerp p1 p2 t)) ps (cdr ps)))])))

(define (bezier-quad-tangent c t)
  (define (fold* t d times ps)
    (cond
      [(= 1 times) (point* (point- (cadr ps) (car ps)) d)]
      [else
       (fold* t d (sub1 times) (zipwith (lambda (p1 p2) (point-lerp p1 p2 t)) ps (cdr ps)))]))
  (let ([d (bezier-quad-degree c)])
    (fold* t d d (bezier-quad-points c))))

;; Custom parametrisation
(define (bezier-quad-parameterization c point-distance)
  (curve-parameterization c bezier-quad-point point-distance))

(define (bezier-quad-inverse-parameterization c point-distance)
  (curve-inverse-parameterization c bezier-quad-point point-distance))

;; Parametrise a curve according to a point evaluation and a point-distance function
(define (curve-parameterize* c curve-point point-distance)
  (let*-values ([(n) (curve-parameterization-samples)]
                [(len lens _)
                 (for/fold ([l 0.0]
                            [s '()]
                            [prev (curve-point c 0)])
                           ([ti (in-range n)])
                   (let* ([p (curve-point c (/ ti (- n 1.0)))]
                          [d (+ l (point-distance prev p))])
                     (values d (cons (cons (sub1 ti) (cons l d)) s) p)))])
    (values n len (cdr (reverse lens)))))

(define (curve-parameterization c curve-point point-distance)
  (let*-values
      ([(n len samples) (curve-parameterize* c curve-point point-distance)]
       [(divs) (- n 1)])
    (lambda (t)
      (cond
        [(<= t 0) 0.0]
        [(>= t 1) len]
        [else
         (let* ([v (* t divs)]
                [i (inexact->exact (truncate v))]
                [r (- v i)]
                [li (list-ref samples i)]
                [la (cadr li)]
                [lb (cddr li)])
           (+ la (* r (- lb la))))]))))

(define (curve-inverse-parameterization c curve-point point-distance)
  (let*-values
      ([(n len samples) (curve-parameterize* c curve-point point-distance)]
       [(divs) (- n 1)])
    (lambda (l-norm)
      (cond
        [(<= l-norm 0) 0.0]
        [(>= l-norm 1) 1.0]
        [else
         (let* ([l (* l-norm len)]
                [ivl (findf (lambda (s) (and (<= (cadr s) l) (< l (cddr s)))) samples)]
                [i (car ivl)]
                [min-l (cadr ivl)]
                [max-l (cddr ivl)])
           (/ (+ i (/ (- l min-l) (- max-l min-l))) divs))]))))
