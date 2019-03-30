; ***************************************************************     ********************************* L ****
; It is just simple library for the complex numbers operations ******* #N is the abstraction barriers   ↓    *
; ***************************************************************     ********************************* H  ***
;                                                                                                      *****
; Here's definition of a pair constructor and its selectors                                                   #1

(define (twin a b)
  (lambda (x)
          (cond ((= x 0) a)
                ((= x 1) b))))

(define (carr twin)
  (twin 0))

(define (cdrr twin)
  (twin 1))

; Here's the constructor for complex numbers and its selectors (syntactic sugar)                              #2

(define (make-COM-rect real-part imag-part type)
  (twin type (twin real-part imag-part)))

(define (real--part z)
  (carr (cdrr z)))

(define (imag--part z)
  (cdrr (cdrr z)))

(define (make-COM-polar magnitude angle type)                                                                 ;#2/eq
  (twin type (twin magnitude angle)))

(define (magnitudee z)                                                                                        ;#2/eq
  (carr (cdrr z)))

(define (anglee z)                                                                                            ;#2/eq
  (cdrr (cdrr z)))

(define (make-complex a b type)
  (cond ((eq? type 'rectengular)
         (make-COM-rect a b type))
        ((eq? type 'polar)
         (make-COM-polar a b type))))

(define (type z)
  (carr z))

; And here is definition of sum, substraction, division and multiplication for complex numbers                 #3
; Polar -> Rectengular because it is more comfortable

(define (+COM-rect z1 z2)
  (make-COM-rect (+ (real--part z1) (real--part z2))
                 (+ (imag--part z1) (imag--part z2))
                 'rectengular))

(define (-COM-rect z1 z2)
  (make-COM-rect (- (real--part z1) (real--part z2))
                 (- (imag--part z1) (imag--part z2))
                 'rectengular))

(define (*COM-rect z1 z2)
  (make-COM-rect (- (* (real--part z1) (real--part z2)) (* (imag--part z1) (imag--part z2)))
                 (+ (* (real--part z1) (imag--part z2)) (* (real--part z2) (imag--part z1)))
                 'rectengular))

(define (/COM-rect z1 z2)
  (make-COM-rect (/ (+ (* (real--part z1) (real--part z2)) (* (imag--part z1) (imag--part z2))) (+ (* (real--part z2) (real--part z2)) (* (imag--part z2) (imag--part z2))))
                 (/ (- (* (real--part z2) (imag--part z1)) (* (real--part z1) (imag--part z2))) (+ (* (real--part z2) (real--part z2)) (* (imag--part z2) (imag--part z2))))
                 'rectengular))

(define (*COM-polar z1 z2)
  (make-COM-polar (* (magnitudee z1) (magnitudee z2))                                                         ;#3/eq
                  (+ (anglee z1) (anglee z2))
                  'polar))

(define (/COM-polar z1 z2)                                                                                    ;#3/eq
  (make-COM-polar (/ (magnitudee z1) (magnitudee z2))
                  (- (anglee z1) (anglee z2))
                  'polar))

;

(define (+c z1 z2)
  (cond ((and (eq? (type z1) (type z2)) (and (eq? (type z1) 'rectengular) (eq? (type z2) 'rectengular)))
         (+COM-rect z1 z2))
        (else (if (eq? (type z1) 'polar)
                  (+c (convert 'polar 'rectengular z1 z2) z2)
                  (+c z1 (convert 'polar 'rectengular z1 z2))))))

(define (-c z1 z2)
  (cond ((and (eq? (type z1) (type z2)) (and (eq? (type z1) 'rectengular) (eq? (type z2) 'rectengular)))
         (-COM-rect z1 z2))
        (else (if (eq? (type z1) 'polar)
                  (-c (convert 'polar 'rectengular z1 z2) z2)
                  (-c z1 (convert 'polar 'rectengular z1 z2))))))

(define (*c z1 z2)
  (cond ((and (eq? (type z1) (type z2)) (and (eq? (type z1) 'rectengular) (eq? (type z2) 'rectengular)))
         (*COM-rect z1 z2))
        ((and (eq? (type z1) (type z2)) (and (eq? (type z1) 'polar) (eq? (type z2) 'polar)))
         (*COM-polar z1 z2))
        (else (if (eq? (type z1) 'polar)
                  (*c (convert 'polar 'rectengular z1 z2) z2)
                  (*c z1 (convert 'polar 'rectengular z1 z2))))))

(define (/c z1 z2)
  (cond ((and (eq? (type z1) (type z2)) (and (eq? (type z1) 'rectengular) (eq? (type z2) 'rectengular)))
         (/COM-rect z1 z2))
        ((and (eq? (type z1) (type z2)) (and (eq? (type z1) 'polar) (eq? (type z2) 'polar)))
         (/COM-polar z1 z2))
        (else (if (eq? (type z1) 'polar)
                  (/c (convert 'polar 'rectengular z1 z2) z2)
                  (/c z1 (convert 'polar 'rectengular z1 z2))))))

; Here's just additional procedures for printing the value of complex numbers and auto-casting                    #*

(define (print-COM-rect z)
  (printf "z = ~a + ~ai" (real--part z) (imag--part z)))

(define (print-COM-polar z)
  (printf "z = ~ae^i~a" (magnitudee z) (anglee z)))

(define (print-complex z)
  (cond ((eq? (type z) 'rectengular)
         (print-COM-rect z))
        ((eq? (type z) 'polar)
         (print-COM-polar ))))

(define (polar->rect t1)
  (make-complex (* (magnitudee t1) (cos (anglee t1))) (* (magnitudee t1) (sin (anglee t1))) 'rectengular))

(define (rect->polar t1)
  (make-complex (sqrt (+ (* (real-part t1) (real-part t1)) (* (imag-part t1) (imag-part t1))))
                (atan (/ (imag-part t1) (real-part t1)))
                'polar))

(define (convert t1 t2 z1 z2)
  (cond ((and (not (eq? (type z1) t2)) (eq? t1 'polar))
         (polar->rect z1))
        ((and (not (eq? (type z1) t2)) (eq? t1 'rectengular))
         (rect->polar z1))
        ((and (not (eq? (type z2) t2)) (eq? t1 'polar))
         (polar->rect z2))
        ((and (not (eq? (type z2) t2)) (eq? t1 'rectengular))
         (rect->polar z2))))
