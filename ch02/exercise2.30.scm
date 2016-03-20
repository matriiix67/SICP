
(define (square x)
  (* x x))

(define (square-tree items)
  (cond ((null? items) '())
	((not (pair? items)) (square items))
	(else (cons (square-tree (car items))
		    (square-tree (cdr items))))))
