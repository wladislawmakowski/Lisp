; ***************************************************************
; It is just simple library for the rational numbers operations *
; ***************************************************************

; Here's definition of a pair constructor and its selectors

(define (twin a b)
  (lambda (pick)
           (cond ((= pick 0) a)
                 ((= pick 1) b))))

(define (xcor x) (x 0))
(define (ycor x) (x 1))

; Here's the constructor for rational numbers and its selectors

(define (make-RAT n d) (twin n d))
(define (num x) (xcor x))
(define (denom x) (ycor x))

; And here is definition of sum, substraction, division and multiplication for rational numbers 

(define (+RAT x y)
  (make-RAT (+ (* (num x) (denom y)) (* (num y) (denom x)))
            (* (denom x) (denom y))))

(define (-RAT x y)
  (make-RAT (- (* (num x) (denom y)) (* (num y) (denom x)))
            (* (denom x) (denom y))))

(define (*RAT x y)
  (make-RAT (* (num x) (num y))
            (* (denom x) (denom y))))

(define (/RAT x y)
   (make-RAT (* (num x) (denom y))
             (* (num y) (denom x))))

