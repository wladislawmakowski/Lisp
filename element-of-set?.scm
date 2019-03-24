(define (element-of-set? x set)
        (cond ((null? set) false)
              (= x (car set) true)
              (else (element-of-set? x (cdr set)))))
