(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a2)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cdr l)))))

(define (addend s) (cadr s))

(define (augend s)
  (let ((a (cddr s)))
    (if (= (length a) 1)
	(car a)
	(make-sum-list a))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cdr l))
      (make-product (car l) (make-product-list (cdr l)))))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (let ((m (cddr p)))
    (if (= (length m) 1)
	(car m)
	(make-product-list m))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exp)
  (cond ((=number? base 1) 1)
	((=number? exp 1) base)
	((=number? exp 0) 1)
	(else
	 (list '** base exp))))
	 
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
		       (deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (if (number? (exponent exp))
						 (- (exponent exp) 1)
						 (' (- (exponent exp) 1)))))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))
