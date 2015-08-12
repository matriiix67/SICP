#lang planet neil/sicp

(define x (list (list 1 2) (list 3 4)))

(define (reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))



(define (deep-reverse lst)
  (cond ((null? lst) lst)
        ((not (pair? lst)) lst)
        (else
         (reverse (list (deep-reverse (car lst))
                        (deep-reverse (cdr lst)))))))
  
      


