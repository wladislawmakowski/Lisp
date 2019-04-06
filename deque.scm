;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname deque) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (make-queue)
  (cons '() '()))

(define (insert-fom-rear q x)
  (cond ((null? q)
         (set! (car q) x)
         (set! (cdr q) x)
         q)
        (set (cdr (cdr q)) x)
        q))

(define (insert-from-front q x)
  (set! (cdr x) (car q))
  (set! (car q) x))

(define (delet-from-front q)
  (cond ((null? q)
         (error "ERROR: QUEUE IS EMPTY" q))
        (else
         (set! (car q) (cdr (car q)))
         q)))

(define (delete-from-rear q)
  (define (lst qu)
    (if (eq? (cdr qu) (cdr q))
        (set! (cdr q) (qu))
        (lst (cdr qu))))
  q)

