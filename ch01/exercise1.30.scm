(define (inc n)
  (+ n 1))

(define (identity n) n)

(define (sum term a next b)
  (define (term-iter a result)
    (if (> a b)
	result
	(term-iter (next a) (+ result (term a)))))
  (term-iter a 0))
