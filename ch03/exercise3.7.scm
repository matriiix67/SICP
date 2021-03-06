
(define (make-account balance pwd)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p pwd)) (lambda (x) "Incorrect password"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknow request -- MAKE-ACCOUNT"
		       m))))
  dispatch)


(define (make-joint account old-pwd new-pwd)
  (and (number? (account old-pwd 'withdraw) 0)
       (lambda (pwd m)
	 (if (eq? pwd new-pwd)
	     (account old-pwd m)
	     (account 'bad-pwd 'foo)))))
