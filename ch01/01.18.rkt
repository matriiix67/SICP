#lang racket

(define (fast-multi a b)
  (fast-multi-iter a b 0))

(define (fast-multi-iter a b product)
  (cond ((= b 0) product)
        ((even? b) (fast-multi-iter (* 2 a) (/ b 2) product))
        ((odd? b) (fast-multi-iter a (- b 1) (+ a product)))))
        