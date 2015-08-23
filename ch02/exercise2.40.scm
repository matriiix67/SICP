(load "section2.2.scm")

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define  (prime-sum-pairs-simple n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
