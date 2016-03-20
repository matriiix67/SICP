
(define (square x)
  (* x x))

(define (square-tree tree)
  (tree-map square tree))


(define (tree-map proc tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (proc tree))
	(else (cons (tree-map proc (car tree))
		    (tree-map proc (cdr tree))))))
