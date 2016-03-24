(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (make-cycle x)
  (set-car! (last-pair x) x)
  x)

;;; 计算(last-pair z)会死循环，因为z是一个循环队列。
