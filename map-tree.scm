(define (map-tree func tree)
  (cond ((null? tree) null)                             ; if node is null return null
        ((not (pair? tree)) (func tree))                ; if node is not a pair apply function
        (else (cons (map-tree func (car tree))          
                    (map-tree func (cdr tree))))))
