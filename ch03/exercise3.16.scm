
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(define three (list 'a 'b 'c))

(define second (cons 'a 'b))
(define third (cons 'a 'b))
(define first (cons second third))
(define four (set-car! third second))

(define s (cons third third))
(define seven (cons s s))

(define 1st (list a' b 'c))
(define oo (set-cdr! (cddr 1st) 1st))
