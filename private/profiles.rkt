#lang racket

(require (for-syntax racket/string
                     racket/list)
         racket/format
         racket/fixnum)

(provide/contract
 (profile-directories (parameter/c (listof (list/c symbol? path-string? any/c)))))


(define profile-directories
  (make-parameter
   (list (list 'built-in "../data/compact-profiles/" #t))))

(define known-profiles (make-parameter (make-hasheq)))

(define-syntax err/nodir
  (syntax-rules ()
    [(_ who dir)
     (raise
      (exn:fail:filesystem (~a who ": directory does not exist"
                               "\n  path: " dir)
                           (current-continuation-marks)))]
    [(_ who dir msgs ...)
     (raise
      (exn:fail:filesystem (~a who ": directory does not exist"
                               "\n  path: " dir "\n  " msgs ...)
                           (current-continuation-marks)))]))

(define/contract (add-profile-directory name dir #:recurse? [recurse? #f]
                                        #:append? [append? #t])
  (->* (symbol? path-string?) (#:recurse? any/c #:append? any/c) any/c)
  (cond
    [(directory-exists? dir)
     (cond
       [(assq name (profile-directories))
        (profile-directories
         (map
          (lambda (p) (if (eq? name (car p)) (list name dir recurse?) p))
          (profile-directories)))]
       [append?
        (profile-directories (append (profile-directories) (list (list name dir recurse?))))]
       [else
        (profile-directories (cons (list name dir recurse?) (profile-directories)))])]
    [else (err/nodir 'append-profile-directory dir)]))

(define/contract (remove-profile-directory name)
  (-> symbol? any/c)
  (profile-directories (remf (lambda (d) (eq? name (car d))) (profile-directories))))

#;
(define/contract (is-icc-profile-file? filename)
  (-> path-string? any/c)
  )

(struct parseinfo (port (bytes-left #:mutable) size filename))

(define-syntax (invalid stx)
  (syntax-case stx ()
    [(_ pi msg rest ...)
     (let ([msg (format "invalid profile: ~a~a" (syntax->datum #'msg)
                        (string-join
                         (make-list (+ 2 (/ (length (syntax-e #'(rest ...))) 2)) "\n  ~a: ~a")
                         ""))])
       (with-syntax ([msg (datum->syntax #'stx msg)])
         #'(raise-user-error
            'parse-icc msg
            "file" (parseinfo-filename pi)
            "at position" (- (parseinfo-size pi) (parseinfo-bytes-left pi) -1)
            rest ...)))]))

(define (check-read pi n)
  (when (< (parseinfo-bytes-left pi) n)
    (invalid pi "cannot read past profile size"
             "profile size" (parseinfo-size pi)
             "requested bytes" n)))

;; NOTE: ICC profiles always store data in big-endian order
(define-syntax-rule (make-int-reader/raw p width)
  (let rec ([n width] [v 0])
    (if (= 0 n) v (rec (fx- n 1) (bitwise-ior (arithmetic-shift v 8) (read-byte p))))))

(define-syntax-rule (bytes-left- pi s)
  (set-parseinfo-bytes-left! pi (fx- (parseinfo-bytes-left pi) s)))

(define-syntax-rule (make-int-reader width)
  (lambda (pi)
    (check-read pi width)
    (let* ([p (parseinfo-port pi)]
           [out (make-int-reader/raw p width)])
      (bytes-left- pi width)
      out)))

(define (read-bcd/digits pi ndigits)
  (unless (even? ndigits)
    (raise-argument-error 'read-bcd/digits "even?" 1 pi ndigits))
  (let ([p (parseinfo-port pi)])
    (let rec ([n ndigits])
      (if (= 0 n)
          null
          (let ([b (read-byte p)])
            (cons (arithmetic-shift b -4) (cons (bitwise-and b #xf) (rec (fx- n 2)))))))))

(define read-int8 (make-int-reader 1))
(define read-int16 (make-int-reader 2))
(define read-int32 (make-int-reader 4))
(define read-int64 (make-int-reader 8))

(define (read-profile-size p) (make-int-reader/raw p 4))

(define (read-bytes* pi n)
  (check-read pi n)
  (let ([val (read-bytes n (parseinfo-port pi))])
    (bytes-left- pi n)
    val))

(define ignore-bytes read-bytes*)

(define (read-profile-version pi)
  (let ([major (read-int8 pi)]
        [minor (filter-not zero? (read-bcd/digits pi 2))]
        [fixrev (read-bcd/digits pi 2)])
    (ignore-bytes pi 1)
    `(,major ,@minor ,@fixrev)))

(define (read-profile-type pi)
  (let ([type-bstr (read-bytes* pi 4)])
    (case type-bstr
      [(#"scnr") 'input-device-profile]
      [(#"mntr") 'display-device-profile]
      [(#"prtr") 'output-device-profile]
      [(#"link") 'devicelink-profile]
      [(#"spac") 'colorspace-conversion-profile]
      [(#"abst") 'abstract-profile]
      [(#"nmcl") 'named-colour-profile])))

(define (read-profile-input-space pi)
  (let ([space-bstr (read-bytes* pi 4)])
    (case space-bstr
      [(#"XYZ ") 'XYZ]
      [(#"Lab ") 'lab]
      [(#"Luv ") 'luv]
      [(#"YCbr") 'YCbCr]
      [(#"Yxy ") 'Yxy]
      [(#"RGB ") 'rgb]
      [(#"GRAY") 'gray]
      [(#"HSV ") 'hsv]
      [(#"HLS ") 'hls]
      [(#"CMYK") 'cmyk]
      [(#"CMY ") 'cmy]
      [(#"2CLR") '2colour]
      [(#"3CLR") '3colour]
      [(#"4CLR") '4colour]
      [(#"5CLR") '5colour]
      [(#"6CLR") '6colour]
      [(#"7CLR") '7colour]
      [(#"8CLR") '8colour]
      [(#"9CLR") '9colour]
      [(#"ACLR") '10colour]
      [(#"BCLR") '11colour]
      [(#"CCLR") '12colour]
      [(#"DCLR") '13colour]
      [(#"ECLR") '14colour]
      [(#"FCLR") '15colour])))

(define (read-profile-pcs pi)
  (let ([space-bstr (read-bytes* pi 4)])
    (case space-bstr
      [(#"XYZ ") 'XYZ]
      [(#"Lab ") 'lab])))

(define (check-signature pi)
  (let ([sig-bstr (read-bytes* pi 4)])
    (unless (bytes=? sig-bstr #"acsp")
      (invalid pi "profile signature must be the bytes 'acsp'"
               "actual" sig-bstr))))

(define (read-illuminant pi)
  )

(define (parse-icc pi)
  (check-read pi 16)
  (read-int32 pi))

(define/contract (load-profile filename)
  (-> path-string? any/c)
  (call-with-input-file filename #:mode 'binary
    (lambda (p)
      (define size (read-profile-size p))
      (define pi (parseinfo p (fx- size 4) size filename))
      (ignore-bytes pi 4)               ; preferred CMM
      (define ver (read-profile-version pi))
      (define type (read-profile-type pi))
      (define input-space (read-profile-input-space pi))
      (define output-space (read-profile-pcs pi))
      (ignore-bytes pi 12)     ; Date-time
      (check-signature pi)
      (ignore-bytes pi 24)     ; Platform & device info
      (ignore-bytes pi 4)      ; Rendering intent. Only `perceptual' is supported
      (displayln (cons input-space output-space))
      (check-read pi 477))))
