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

(define (make-COM-rect real-part imag-part)
  (twin real-part imag-part))

(define (real--part z)
  (carr z))

(define (imag--part z)
  (cdrr z))

(define (make-COM-polar magnitude angle)                                                                      ;#2/eq
  (twin magnitude angle))

(define (magnitudee z)                                                                                        ;#2/eq
  (carr z))

(define (anglee z)                                                                                            ;#2/eq
  (cdrr z))

; And here is definition of sum, substraction, division and multiplication for complex numbers                 #3

(define (+COM-rect z1 z2)
  (make-COM-rect (+ (real--part z1) (real--part z2))
                 (+ (imag--part z1) (imag--part z2))))

(define (-COM-rect z1 z2)
  (make-COM-rect (- (real--part z1) (real--part z2))
                 (- (imag--part z1) (imag--part z2))))

(define (*COM-rect z1 z2)
  (make-COM-rect (- (* (real--part z1) (real--part z2)) (* (imag--part z1) (imag--part z2)))
                 (+ (* (real--part z1) (imag--part z2)) (* (real--part z2) (imag--part z1)))))

(define (/COM-rect z1 z2)
  (make-COM-rect (/ (+ (* (real--part z1) (real--part z2)) (* (imag--part z1) (imag--part z2))) (+ (* (real--part z2) (real--part z2)) (* (imag--part z2) (imag--part z2))))
                 (/ (- (* (real--part z2) (imag--part z1)) (* (real--part z1) (imag--part z2))) (+ (* (real--part z2) (real--part z2)) (* (imag--part z2) (imag--part z2))))))

(define (*COM-polar z1 z2)
  (make-COM-polar (* (magnitudee z1) (magnitudee z2))                                                         ;#3/eq
                  (+ (anglee z1) (anglee z2))))

(define (/COM-polar z1 z2)                                                                                    ;#3/eq
  (make-COM-polar (/ (magnitudee z1) (magnitudee z2))
                  (- (anglee z1) (anglee z2))))

; Here's just additional procedures for printing the value of complex numbers                                  #*

(define (print-COM-rect z)
  (printf "z = ~a + ~ai" (real--part z) (imag--part z)))

(define (print-COM-polar z)
  (printf "z = ~ae^i~a" (magnitudee z) (anglee z)))
