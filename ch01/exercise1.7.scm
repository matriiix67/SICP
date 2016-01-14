#lang planet neil/sicp

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))


(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? new-guess old-guess)
  (> 0.01 (/ (abs (- new-guess old-guess)) old-guess)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
