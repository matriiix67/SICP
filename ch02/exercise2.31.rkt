#lang planet neil/sicp

(define (square x) (* x x))

(define (tree-map square tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map square sub-tree)
             (square sub-tree)))
       tree))
