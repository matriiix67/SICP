
;;; 3.25 实际上就是要我们实现n维表

(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))


