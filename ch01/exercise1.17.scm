
(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-multi a b)
  (cond ((= b 1) a)
	((even? b) (fast-multi (double a) (halve b)))
	(else (+ a (fast-multi a (- b 1))))))
