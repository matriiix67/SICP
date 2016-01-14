(load "ch1.scm")

(define (bigger x y)
  (if (> x y)
      x
      y))

(define (smaller x y)
  (if (< x y)
      x
      y))

(define (max-sum-of-square x y z)
  (sum-of-squares (bigger x y) (bigger (smaller x y) z)))
	 
	
