
(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(define (cycle? x)
  (define visited nil)
  (define (iter x)
    (set! visited (cons x visited))
    (cond ((null? (cdr x)) false)
	  ((memq (cdr x) visited) true)
	  (else (iter (cdr x)))))
  (iter x))
