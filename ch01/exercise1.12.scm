
(define (pascal row col)
  (cond ((> col row) (error "unvalue col value"))
	((or (= col 0) (= col row)) 1)
	(else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))
