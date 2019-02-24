(define (foreach f l)
  (cond ((null? l) 0)
        ((f (car l))
         (foreach f (cdr l)))))
