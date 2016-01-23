
(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse list)
  (if (null? (cdr list))
      list
      (append (reverse (cdr list))
	      (cons (car list) nil))))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	(reverse answer)
	(iter (cdr things) (cons (square (car things))
				 answer))))
  (iter items nil))
