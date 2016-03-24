;; 数据结构，链表判断是否有环，利用两个指针，一个走一步，一个走两步，如果走完之前有相遇则有环，否则没有
(define (list-walk step lst)
  (cond ((null? lst) '())
	((= step 0) lst)
	(else (list-walk (- step 1) (cdr lst)))))

(define (cycle? lst)
  (define (iter x y)
    (let ((one-walk (list-walk 1 x))
	  (two-walk (list-walk 2 y)))
      (cond ((or (null? one-walk) (null? two-walk)) false)
	    ((eq? one-walk two-walk) true)
	    (else (iter one-walk two-walk)))))
  (iter lst lst))
