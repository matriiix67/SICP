(define (square x) (* x x))

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))

(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define dx 0.00001))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx))) 3)))

;; test
((smooth square) 5)

;; n times smooth
((repeated (smooth square) 2) 5)
