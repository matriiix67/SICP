#lang planet neil/sicp

(define (for-each op lst)
  (if (not (null? lst))
      (and  ;; can use begin/and/or
        (op (car lst))
        (for-each op (cdr lst)))))

