
;; +1
(define (inc n) (+ n 1))

;; square
(define (square x) (* x x))

;; for test
(define (identity x) x)

(define (smalllest-div n)
  (define (divides? a b)
    (= 0 (remainder b a)))
  (define (find-div n test)
    (cond ((> (square test) n) n) ((divides? test n) test)
	  (else (find-div n (+ test 1)))))
  (find-div n 2))

(define (prime? n)
  (if (= n 1) #f (= n (smalllest-div n))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
	  (filtered-accumulate combiner null-value term (next a) next b filter))))

;; a)
(define (sum-of-prime-square a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (gcd m n)
  (if (= n 0)
      m
      (gcd n (remainder m n))))

(define (relative-prime? m n)
  (= (gcd m n) 1))

(define (product-of-prime-relative n)
  (define (filter x)
    (relative-prime? x n))
  (filtered-accumulate * 1 identity 1 inc n filter))
