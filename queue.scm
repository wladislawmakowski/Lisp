;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname queue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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