(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))


;;(1 (2 (3 4)))
;;      ^
;;    /   \
;;   1     ^ (2 (3 4))
;;       /   \
;;      2     ^ (3 4)
;;          /   \
;;         3     4
