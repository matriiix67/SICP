
(define (inc n) (+ n 1))
(define (square x) (* x x))

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))
