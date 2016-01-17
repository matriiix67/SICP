
(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


;; exercise 1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 10.0)

