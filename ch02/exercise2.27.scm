
(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse list)
  (if (null? (cdr list))
      list
      (append (reverse (cdr list))
	      (cons (car list) nil))))

(define (deep-reverse li) 
   (cond ((null? li) '()) 
         ((not (pair? li)) li) 
         (else (append (deep-reverse (cdr li))  
                       (list (deep-reverse (car li)))))))

(define x (list (list 1 2) (list 3 4)))
