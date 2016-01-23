(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (same-parity a . b)
  (define (same-parity-iter source dist remainder-val)
    (if (null? source)
	dist
	(same-parity-iter (cdr source)
			  (if (= (remainder (car source) 2) remainder-val)
			      (append dist (list (car source)))
			      dist)
			  remainder-val)))
  (same-parity-iter b (list a) (remainder a 2)))
