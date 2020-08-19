#lang racket/base

(require (for-syntax math/flonum
                     racket/base
                     racket/stxparam)
         math/flonum
         racket/stxparam
         "./helpers.rkt")

(provide expand-trc-func expand-trc-inverse-func)

;; Various procedures to linearize RGB color-space according to parametric curve types
;; defined in the standard ICC.1:2010 (Profile version
;; 4.3.0.0) http://www.color.org/specification/ICC1v43_2010-12.pdf (pages 69, 70)

(define-syntax-parameter _component #f)

(define-syntax (expand-trc-func stx)
  (syntax-case stx ()
    [(_ (ir ig ib g))
     #'(expand-trc-func
        (ir ig ib g)
        ()
        (flexpt _component g))]
    [(_ (ir ig ib g a b))
     (with-syntax ([(t) (generate-temporaries #'a)])
       #'(expand-trc-func
          (ir ig ib g a b)
          ([t (fl/ (fl- b) a)])
          (if (fl>= _component t)
              (flexpt (fl+ (fl* a _component) b) g)
              0.0)))]
    [(_ (ir ig ib g a b c))
     (with-syntax ([(t) (generate-temporaries #'a)])
       #'(expand-trc-func
          (ir ig ib g a b c)
          ([t (fl/ (fl- b) a)])
          (if (fl>= _component t)
              (fl+ (flexpt (fl+ (fl* a _component) b) g) c)
              c)))]
    [(_ (ir ig ib g a b c d))
     #'(expand-trc-func
        (ir ig ib g a b c d)
        ()
        (if (fl>= _component d)
            (flexpt (fl+ (fl* a _component) b) g)
            (fl* c _component)))]
    [(_ (ir ig ib g a b c d e f))
     #'(expand-trc-func
        (ir ig ib g a b c d e f)
        ()
        (if (fl>= _component d)
            (fl+ (flexpt (fl+ (fl* a _component) b) g) c)
            (fl+ (fl* c _component) f)))]
    [(_ (ir ig ib arg ...) (bind ...) expr)
     (with-syntax ([(cvar ...) #'(ir ig ib)]
                   [(comp ...) (generate-temporaries (list #'ir #'ig #'ib))])
       #'(let ([comp cvar] ...)
           (let (bind ...)
             (values
              (let* ([comp (norm/clamp comp)]
                     [comp
                      (syntax-parameterize ([_component (make-rename-transformer #'comp)])
                        expr)])
                (clamp comp))
              ...))))]
    [(_ (_ ...))
     (raise-syntax-error
      #f
      "unknown icc tone reproduction curve type.\n  expected: 1, 3, 4, 5, or 7 arguments"
      stx)]))

(define-syntax (expand-trc-inverse-func stx)
  (syntax-case stx ()
    [(_ (ir ig ib g))
     #'(expand-trc-inverse-func
        (ir ig ib g)
        ()
        (flexpt _component (fl/ 1. g)))]
    [(_ (ir ig ib g a b))
     (with-syntax ([(t) (generate-temporaries #'a)])
       #'(expand-trc-inverse-func
          (ir ig ib g a b)
          ()
          ;; When (= y 0) ? This cannot be defined properly?
          (fl/ (fl- (flexpt _component (fl/ 1. g)) b) a)))]
    [(_ (ir ig ib g a b c))
     (with-syntax ([(t) (generate-temporaries #'a)])
       #'(expand-trc-inverse-func
          (ir ig ib g a b c)
          ()
          ;; When (= y c) ? This cannot be defined properly?
          (fl/ (fl- (flexpt (fl- _component c) (fl/ 1. g)) b) a)))]
    [(_ (ir ig ib g a b c d))
     #'(expand-trc-inverse-func
        (ir ig ib g a b c d)
        ()
        (if (fl>= _component (fl* d c))
            (fl/ (fl- (flexpt _component (fl/ 1. g)) b) a)
            (fl/ _component c)))]
    [(_ (ir ig ib g a b c d e f))
     #'(expand-trc-inverse-func
        (ir ig ib g a b c d e f)
        ()
        (if (fl>= _component (fl+ (fl* c d) f))
            (fl/ (fl- (flexpt (fl- _component c) (fl/ 1. g)) b) a)
            (fl/ (fl- _component f) c)))]
    [(_ (ir ig ib arg ...) (bind ...) expr)
     (with-syntax ([(cvar ...) #'(ir ig ib)]
                   [(comp ...) (generate-temporaries (list #'ir #'ig #'ib))])
       #'(let ([comp cvar] ...)
           (let (bind ...)
             (values
              (let* ([comp (clamp comp)]
                     [comp
                      (syntax-parameterize ([_component (make-rename-transformer #'comp)])
                        expr)])
                (unnorm/clamp comp))
              ...))))]
    [(_ (_ ...))
     (raise-syntax-error
      #f
      "unknown icc tone reproduction curve type.\n  expected: 1, 3, 4, 5, or 7 arguments"
      stx)]))
