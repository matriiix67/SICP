(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
	     (begin (set! x (rand-update x)) x))
	    ((eq? m 'reset)
	     (lambda (new-value) (begin (set! x new-value) x)))))
    dispatch))

;;; for test
(define random-init 0)
(define (rand-update x) (+ x 1))
