#lang racket/base

(require math/flonum
         racket/unsafe/ops)

(provide (except-out (all-defined-out) 3x3-check 3x3->values/unsafe))


(define-syntax-rule (->fl* a ...) (values (real->double-flonum a) ...))

(define (make-3x3 m00 m01 m02 m10 m11 m12 m20 m21 m22)
  (let-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22)
                (->fl* m00 m01 m02 m10 m11 m12 m20 m21 m22)])
    (flvector m00 m01 m02 m10 m11 m12 m20 m21 m22)))

(define-syntax-rule (3x3-check who m)
  (unless (and (flvector? m) (= (unsafe-flvector-length m) 9))
    (raise-argument-error who "flvector? of 9 elements (3x3 matrix)" m)))

(define (3x3-identity)
  (flvector 1. 0. 0.
            0. 1. 0.
            0. 0. 1.))

(define (3x3-diag d1 d2 d3)
  (flvector d1 0. 0.
            0. d2 0.
            0. 0. d3))

;; [unsafe] Must check argument first using 3x3-check
(define-syntax-rule (3x3->values/unsafe m)
  (values (unsafe-flvector-ref m 0)
          (unsafe-flvector-ref m 1)
          (unsafe-flvector-ref m 2)
          (unsafe-flvector-ref m 3)
          (unsafe-flvector-ref m 4)
          (unsafe-flvector-ref m 5)
          (unsafe-flvector-ref m 6)
          (unsafe-flvector-ref m 7)
          (unsafe-flvector-ref m 8)))

(define (3x3-ref m r c) (flvector-ref m (+ (* r  3) c)))

(define (3x3-det m)
  (3x3-check '3x3-det m)
  (let-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22) (3x3->values/unsafe m)])
    (fl+ (fl* m00 m11 m22) (fl- (fl* m00 m12 m21))
         (fl* m01 m12 m20) (fl- (fl* m01 m10 m22))
         (fl* m02 m10 m21) (fl- (fl* m02 m11 m20)))))

(define (3x3-invertible? m)
  (and (flvector? m)
       (= (unsafe-flvector-length m) 9)
       (not (= 0 (let-values ([(m00 m01 m02 m10 m11 m12 m20 m21 m22) (3x3->values/unsafe m)])
                   (fl+ (fl* m00 m11 m22) (fl- (fl* m00 m12 m21))
                        (fl* m01 m12 m20) (fl- (fl* m01 m10 m22))
                        (fl* m02 m10 m21) (fl- (fl* m02 m11 m20))))))))


; matrix * column-vector. Returns 3 values
(define (3x3*vec m a b c)
  (3x3-check '3x3*vec m)
  (let-values
      ([(m00 m01 m02 m10 m11 m12 m20 m21 m22) (3x3->values/unsafe m)]
       [(a b c) (->fl* a b c)])
    (values
     (fl+ (fl* m00 a) (fl* m01 b) (fl* m02 c))
     (fl+ (fl* m10 a) (fl* m11 b) (fl* m12 c))
     (fl+ (fl* m20 a) (fl* m21 b) (fl* m22 c)))))

; row-vector * matrix. Returns 3 values
(define (vec*3x3 a b c m)
  (3x3-check 'vec*3x3 m)
  (let-values
      ([(m00 m01 m02 m10 m11 m12 m20 m21 m22) (3x3->values/unsafe m)]
       [(a b c) (->fl* a b c)])
    (values
     (fl+ (fl* a m00) (fl* b m10) (fl* c m20))
     (fl+ (fl* a m01) (fl* b m11) (fl* c m21))
     (fl+ (fl* a m02) (fl* b m12) (fl* c m22)))))

(define (3x3* ma mb)
  (3x3-check '3x3* ma)
  (3x3-check '3x3* mb)
  (let-values
      ([(ma00 ma01 ma02 ma10 ma11 ma12 ma20 ma21 ma22) (3x3->values/unsafe ma)]
       [(mb00 mb01 mb02 mb10 mb11 mb12 mb20 mb21 mb22) (3x3->values/unsafe mb)])
    (flvector
     (fl+ (fl* ma00 mb00) (fl* ma01 mb10) (fl* ma02 mb20))
     (fl+ (fl* ma00 mb01) (fl* ma01 mb11) (fl* ma02 mb21))
     (fl+ (fl* ma00 mb02) (fl* ma01 mb12) (fl* ma02 mb22))

     (fl+ (fl* ma10 mb00) (fl* ma11 mb10) (fl* ma12 mb20))
     (fl+ (fl* ma10 mb01) (fl* ma11 mb11) (fl* ma12 mb21))
     (fl+ (fl* ma10 mb02) (fl* ma11 mb12) (fl* ma12 mb22))

     (fl+ (fl* ma20 mb00) (fl* ma21 mb10) (fl* ma22 mb20))
     (fl+ (fl* ma20 mb01) (fl* ma21 mb11) (fl* ma22 mb21))
     (fl+ (fl* ma20 mb02) (fl* ma21 mb12) (fl* ma22 mb22)))))

(define (3x3-mult-columns m a b c)
  (3x3-check '3x3*vec m)
  (let-values
      ([(m00 m01 m02 m10 m11 m12 m20 m21 m22) (3x3->values/unsafe m)]
       [(a b c) (->fl* a b c)])
    (flvector (fl* m00 a) (fl* m01 b) (fl* m02 c)
              (fl* m10 a) (fl* m11 b) (fl* m12 c)
              (fl* m20 a) (fl* m21 b) (fl* m22 c))))

(define (3x3-inverse m)
  (3x3-check '3x3-inverse m)
  (let*-values
      ([(m00 m01 m02 m10 m11 m12 m20 m21 m22) (3x3->values/unsafe m)]
       [(det) (fl+ (fl* m00 m11 m22) (fl- (fl* m00 m12 m21))
                   (fl* m01 m12 m20) (fl- (fl* m01 m10 m22))
                   (fl* m02 m10 m21) (fl- (fl* m02 m11 m20)))])
    (if (fl= det 0.0)
        (raise-argument-error '3x3-inverse "matrix is not invertible" m)
        (let ([rdet (fl/ 1.0 det)])
          (flvector
           (fl* (fl- (fl* m11 m22) (fl* m12 m21)) rdet)
           (fl* (fl- (fl* m02 m21) (fl* m01 m22)) rdet)
           (fl* (fl- (fl* m01 m12) (fl* m02 m11)) rdet)

           (fl* (fl- (fl* m12 m20) (fl* m10 m22)) rdet)
           (fl* (fl- (fl* m00 m22) (fl* m02 m20)) rdet)
           (fl* (fl- (fl* m02 m10) (fl* m00 m12)) rdet)

           (fl* (fl- (fl* m10 m21) (fl* m11 m20)) rdet)
           (fl* (fl- (fl* m01 m20) (fl* m00 m21)) rdet)
           (fl* (fl- (fl* m00 m11) (fl* m01 m10)) rdet))))))
