

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (abs-simple x)
  (cond ((< x 0) (- x))
	(else x)))

(define (abs-if x)
  (if (< x 0)
      (- x)
      x))
