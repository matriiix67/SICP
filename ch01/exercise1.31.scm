
(define (inc n) (+ n 1))

(define (identity x) x)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


(define (factorial n)
  (product identity 1 inc n))

(define (even? n)
  (= (remainder n 2) 0))

(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))
