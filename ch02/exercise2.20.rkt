#lang planet neil/sicp

(define (same-parity first . etc)
  (define (sp x result)
    (cond ((null? x) result)
          ((even? (- first (car x)))
           (sp (cdr x) (append result (list (car x)))))
          (else
           (sp (cdr x) result))))
  (sp etc (list first)))
      
          