#lang planet neil/sicp

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

;; 费马检测
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; carmichael数检测
(define (carmichael? n)
  (define (carmichael-iter a n)
    (if (= a n)
        #t
        (if (= (expmod a n n) a)
            (carmichael-iter (+ a 1) n)
            #f)))
  (carmichael-iter 1 n))
 
(define (carmichael-test n expected)
  (define (report-result n result expected)
    (newline)
    (display n)
    (display " : ")
    (display result)
    (display " : ")
    (display (if (eq? result expected) "OK" "FOOLED")))
  (report-result n (carmichael? n) expected))

