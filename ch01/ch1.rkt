#lang racket


;; SECTION 1.1.4

(define (square x) (* x x))

;; section 1.2.5

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; section 1.2.6

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;:(define (prime? n)
;:  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;;exercise 1.21
;: (smallest-divisor 199)
;: (smallest-divisor 1999)
;: (smallest-divisor 19999)

;;;exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      #f)) ;; 这里因为Racket实现不允许if只有一个分支,所以用#f来表示另一个分支

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (search-for-primes start count)
  (cond ((= count 0) (newline))
        ((timed-prime-test start)
         (search-for-primes (next-odd start) (- count 1)))
        (else (search-for-primes (next-odd start) count))))

(define (next-odd n)
  (if (= (remainder n 2) 0)
      (+ n 1)
      (+ n 2)))

;;;exercise 1.23

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
      
(define (fast-smallest-divisor n)
  (fast-find-divisor n 2))

(define (fast-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (fast-smallest-divisor n)))


;;;exercise 1.24


;;;exercise 1.25


;;;section 1.3.1

(define (cube x) (* x x x))

;;(define (sum-integers a b)
;;  (if (> a b)
;;      0
;;      (+ a (sum-integers (+ a 1) b))))

;;(define (sum-cubes a b)
;;  (if (> a b)
;;      0
;;      (+ (cube a) (sum-cubes (+ a 1) b))))

;;(define (pi-sum a b)
;;  (if (> a b)
;;     0
;;      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))


(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


;;;exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;;;exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))


(define (dec x) (- x 1))
(define (f x)
  (* (/ (dec x) x)
     (/ (inc x) x)))
(define (plus-2 x) (+ x 2.0))
(define (pi n)  (* 4 (product f 3 plus-2 n)))
  
  
    