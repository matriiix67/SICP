#lang planet neil/sicp

<<<<<<< HEAD
=======
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))




(define (total-weight mobile)
  (cond ((null? mobile) '())
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))
        

(define mobile (make-mobile (make-branch 10 20) (make-branch 10 20)))

(define (branch-weight branch)
    (if (hangs-another-mobile? branch)              ; 如果分支吊着另一个活动体
        (total-weight (branch-structure branch))    ; 那么这个活动体的总重量就是这个分支的重量
        (branch-structure branch)))                 ; 否则, 分支的 structure 部分就是分支的重量

(define (hangs-another-mobile? branch)              ; 检查分支是否吊着另一个活动体
    (pair? (branch-structure branch)))

(define (balance? tree)
  (cond ((or (null? tree) (not (pair? tree))) #t)
        ((and (balance? (branch-structure (left-branch tree)))
             (balance? (branch-structure (right-branch tree)))
             (= (* (branch-length (left-branch tree))
                   (branch-weight (left-branch tree)))
                (* (branch-length (right-branch tree))
                   (branch-weight (right-branch tree))))) #t)
        (else #f)))

;; d) 只需要把cadr换成cdr即可。
>>>>>>> origin/master
