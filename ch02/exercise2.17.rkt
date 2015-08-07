#lang planet neil/sicp

(define (last-pair list)
  (cond ((null? list) (error "list is empty!"))
        ((null? (cdr list)) (car list))
        (else (last-pair (cdr list)))))

(define odds (list 1 3 5 7))

