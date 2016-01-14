
(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))


(define (fast-multi a b)
  (fast-multi-iter a b 0))

(define (fast-multi-iter a b c)
  (cond ((= b 0) c)
	((even? b) (fast-multi-iter (double a) (halve b) c))
	(else (fast-multi-iter a (- b 1) (+ a c)))))
