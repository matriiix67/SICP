#lang planet neil/sicp

(define (square x)
  (* x x))

(define (cube-iter guess x)
  (if good-enough? guess x)
  x
  (cube-iter (improve guess x) x))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (cube-root x)
  (cube-iter 1.0 x))