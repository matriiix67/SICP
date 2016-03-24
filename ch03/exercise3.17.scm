(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(define (count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered))
	  0
	  (begin
	    (set! encountered (cons x encountered))
	    (+ (helper (car x))
	       (helper (cdr x))
	       1))))
    (helper x)))
