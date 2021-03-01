#lang racket

;for testing in another file
(provide (all-defined-out))

;empty list: null
;cons constructor: cons
;head of list: car
;tail of list: cdr
;check for empty null?

;sum all the numbers in a list
(define (sum xs)
    (if (null? xs)
        0
        (+ (car xs) (sum (cdr xs)))))

(sum (list 3 4 5 6))

;append
(define (my-append xs ys)
    (if (null? xs)
        ys
        (cons (car xs) (my-append (cdr xs) ys))))

;map
(define (my-map f xs)
    (if (null? xs)
        null
        (cons (f (car xs))
            (my-map f (cdr xs)))))

(define foo (my-map (lambda (x) (+ x 1))
                    (cons 3 (cons 4 (cons 5 null)))))

foo

(define current 0)
(define (count x)
    (let ([current current])
        (+ 1 current)))
        

(map (lambda (x) (count)) (list (list) 1 2 "abc" 4 ))