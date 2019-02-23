(define (node x y)
  (lambda (x) (cond ((= x 0) x)
                    ((= x 1) y))))

(define (carr x) (x 0))
(define (cdrr x) (x 1))

(define (mapp f l)
  (if (null? (cdrr l))
      null
      (node (f (carr l))
        (mapp f (cdrr l)))))
