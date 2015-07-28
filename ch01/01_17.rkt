#lang racket
(define (fast-add a b)
  (cond ((= b 0) 0)
        ((even? b) (* 2 (fast-add a (/ b 2))))
        (else (+ a (fast-add a (- b 1))))))