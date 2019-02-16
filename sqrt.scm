;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sqrt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define improve (lambda (guess x) (/ (+ guess (/ x guess)) 2)))
(define good-enough? (lambda (guess x) (< (- (* guess guess) x) 0.000001)))
(define sqrt-iter (lambda (guess x) (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))))
(definsqrt (lambda (x) (sqrt 1.0 x)))
