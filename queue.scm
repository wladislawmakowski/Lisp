(define (make-queue)
  (cons '() '()))

(define (insert q x)
  (cond ((null? q)
         (set! (car q) x)
         (set! (cdr q) x)
         q)
        (else
         (set! (cdr (cdr q)) x)
         q)))

(define (delete q)
  (cond ((null? q)
         (error "ERROR: QUEUE IS EMPTY" q))
        (else
         (set! (car q) (cdr (car q)))
         q)))
