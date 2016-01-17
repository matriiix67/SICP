(define (inc n) (+ n 1))
(define (square x) (* x x))

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))

(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))
