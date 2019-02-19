(define (pow-core x n) (if (= n 0) 1 (* x (pow-core x (- n 1)))))
(define (pow x n) (pow-core x n))
