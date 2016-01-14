
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (odd? n)
  (= (remainder n 2) 1))

(define (next-odd n)
  (if (odd? n)
      (+ n 2)
      (+ n 1)))


(define (search-for-prime n count)
  (cond ((= count 0) (display " are primes "))
	((prime? n)
	 (display n)
	 (newline)
	 (search-for-prime (next-odd n) (- count 1)))
	(else (search-for-prime (next-odd n) count))))
