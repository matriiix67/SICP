(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (sub-tree)
			 (if (pair? sub-tree)
			     (count-leaves sub-tree)
			     1))
		       tree)))
