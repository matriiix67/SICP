
(define nil '())

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (structure-is-mobile? structure)
  (pair? structure))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (structure-is-mobile? s)
	(total-weight s)
	s)))

(define level-1-mobile (make-mobile (make-branch 2 1) 
                                     (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                     (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                     (make-branch 8 2))) 

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (same-torque? left right)
  (= (branch-torque left)
     (branch-torque right)))

(define (balance? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (same-torque? left right)
	 (brache-balance? left)
	 (branch-balance? right))))

(define (branch-balance? branch)
  (let ((s (branch-structure branch)))
    (if (structure-is-mobile? s)
	(balance? s)
	true)))

 
