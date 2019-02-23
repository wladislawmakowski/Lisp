;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname linked_list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

