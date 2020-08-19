#lang info

(define collection "chroma")

(define version "0.5")

(define pkg-desc "Library for working with colors, color palettes, and color space conversion.")

(define deps '("base" "math-lib" "draw-lib" "pict-lib"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/chroma.scrbl" ())))

(define compile-omit-paths '("test"))

(define test-include-paths '("test"))
(define test-omit-paths '("./info.rkt" "./main.rkt" "private" "scribblings"))

(define pkg-authors '(hkrish))
