(define (cons-stream a b)
    (cons a (aside b)))

(define (aside a)
    (lambda () a))

(define (count a)
    (a))

(define (car-stream s)
    (car s))

(define (cdr-stream s)
    (count (cdr s)))

(define (stream-null? e)
    (null? e))

(define empty-stream '())

(define (map-stream p s)
    (if (stream-null? s)
        empty-stream
        (cons-stream (p (car-stream s)) (map-stream p (cdr-stream s)))))

(define (filter-stream p s)
    (cond ((stream-null? s) empty-stream)
          ((p (car-stream s))
            (cons-stream (car-stream s) (filter-stream p (cdr-stream s))))
          (else (filter-stream p (cdr-stream s)))))

(define (for-each-stream p s)
    (if (null-stream? s)
        '.
        (begin (p (car-stream s))
               (for-each-stream p (cdr-stream s)))))

;(define (sort-ascending s)
;   (define (sort n)
;        (filter-stream (lambda (x) (= x n)) s))
;    (cond ((null-stream? s) empty-stream)
;          (else )))

;(define (sort-ascending s num)
;    (define (sort n)
;        (cond ((null-stream? s) empty-stream)
;              (else (filter-stream (lambda (x) (= x n)) s)))))
;    ())

(define (sort s n)
    (cond ((stream-null? s) empty-stream)
          (cons-stream (filter-stream (lambda (x) (= x n)) s) (sort s (+ n 1)))))

(define (sort-ascending s)
    (sort s 1))

(define (stream-to-list s)
    (if (null? s)
        '()
        (cons (car s) (count (stream-to-list (cdr s))))))

(define (list-to-stream l)
    (if (null? l)
        '()
        (cons-stream (car l) (list-to-stream (cdr l)))))
