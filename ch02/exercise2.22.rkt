#lang planet neil/sicp

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (* (car things) (car things))
                    answer))))
  (iter (reverse items) nil))