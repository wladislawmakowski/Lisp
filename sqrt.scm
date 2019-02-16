(define improve (lambda (guess x) (/ (+ guess (/ x guess)) 2)))
(define good-enough? (lambda (guess x) (< (- (* guess guess) x) 0.000001)))
(define sqrt-iter (lambda (guess x) (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))))
(define sqrt (lambda (x) (sqrt 1.0 x)))
