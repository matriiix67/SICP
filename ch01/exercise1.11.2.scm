
(define (f n)
  (f-iteration 2 1 0 0 n))

(define (f-iteration a b c i n)
  (if (= i n)
      c
      (f-iteration (+ a (* 2 b) (* 3 c)) a b (+ i 1) n)))
