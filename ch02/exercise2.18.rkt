#lang planet neil/sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

(define odds (list 1 3 5 7))

