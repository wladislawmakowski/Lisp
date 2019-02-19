(define (fib x) (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))
(define (fibonacci x) (fib x))
