
(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe items)
  (if (null? items)
      nil
      (if (not (pair? (car items)))
	  (cons (car items) (fringe (cdr items)))
	  (append (fringe (car items)) (fringe (cdr items))))))
  

(define x (list (list 1 2) (list 3 4)))
