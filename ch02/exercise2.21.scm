(define nil '())

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) 
	    (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))
