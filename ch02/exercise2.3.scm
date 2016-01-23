
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
	      (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define (make-rect length-1 length-2 width-1 width-2)
  (cons (cons length-1 length-2)
	(cons width-1 width-2)))

(define (length-1-rect r)
  (car (car r)))

(define (length-2-rect r)
  (cdr (car r)))

(define (width-1-rect r)
  (car (cdr r)))

(define (width-2-rect r)
  (cdr (cdr r)))

(define (length-of-rect r)
  (let ((length (length-1-rect r)))
    (let ((start (start-segment length))
	  (end (end-segment length)))
      (- (x-point end)
	 (x-point start)))))

(define (width-of-rect r)
  (let ((width (width-1-rect r)))
    (let ((start (start-segment width))
	  (end (end-segment width)))
      (- (y-point end)
	 (y-point start)))))


(define (perimeter-rect r)
  (let ((length (length-of-rect r))
	(width (width-of-rect r)))
    (* 2 (+ length width))))

(define (area-rect r)
  (* (length-of-rect r)
     (width-of-rect r)))

(define (print-rect r)
    (let ((l1 (length-1-rect r))
          (l2 (length-2-rect r))
          (w1 (width-1-rect r))
          (w2 (width-2-rect r)))

        (newline)
        (display "Length 1:")
        (print-point (start-segment l1))
        (print-point (end-segment l1))

        (newline)
        (display "Length 2:")
        (print-point (start-segment l2))
        (print-point (end-segment l2))

        (newline)
        (display "Width 1:")
        (print-point (start-segment w1))
        (print-point (end-segment w1))

        (newline)
        (display "Width 2:")
        (print-point (start-segment w2))
        (print-point (end-segment w2))))

(define rect (make-rect (make-segment (make-point 1 4) (make-point 4 4))
			(make-segment (make-point 1 2) (make-point 4 2))
			(make-segment (make-point 1 2) (make-point 1 4))
			(make-segment (make-point 4 2) (make-point 4 4))))
