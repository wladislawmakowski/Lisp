(define (polynom a b c)
  (lambda (pick)
    (cond ((= pick 0) a)
          ((= pick 1) b)
          ((= pick 10) c)
          ((= pick 11) (- (* b b) (* 4 a c)))
          )))

(define (first-coef polynom)
  (polynom 0))

(define (second-coef polynom)
  (polynom 1))

(define (free-member polynom)
  (polynom 10))

(define (discriminant polynom)
  (polynom 11))

(define (first-root polynom)
  (/ (+ (* (second-coef polynom) -1) (sqrt (discriminant polynom)))
     (* (first-coef polynom) 2)))

(define (second-root polynom)
  (/ (- (* (second-coef polynom) -1) (sqrt (discriminant polynom)))
     (* (first-coef polynom) 2)))

(define (print-result polynom)
  (printf "(~a)x^2 + (~a)x + (~a) = 0~nD = ~a~nx1 = ~a~nx2 = ~a" (first-coef polynom) (second-coef polynom) (free-member polynom) (discriminant polynom) (first-root polynom) (second-root polynom)))
