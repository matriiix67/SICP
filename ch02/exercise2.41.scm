(load "section2.2.scm")

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
    (flatmap (lambda (i)
                 (map (lambda (j)                   ; cons 起 i 元素和二元组 j ，组成三元组
                          (cons i j))
                      (unique-pairs (- i 1))))      ; 生成不大于 i 的所有相异整数二元组
             (enumerate-interval 1 n)))             ; 生成 1 至 n 的所有整数，作为 i 

(define (triple-sum-equal-to? sum triple)
  (= sum
     (+ (car triple)
        (cadr triple)
        (caddr triple))))

(define (remove-triples-not-equal-to sum triple)
  (filter (lambda (current-triple)
            (triple-sum-equal-to? sum (current-triple))
            (triple))))