#lang racket

;for testing in another file
(provide (all-defined-out))

;basic definitions
(define s "hello") ; comments
s

(define x 3); val = 3
(define y (+ x 2)); + is a function, call it here

(define cube1
    (lambda (x)
        (* x (* x x))));

(define cube2
    (lambda (x)
        (* x x x)));

(define (cube3 x)
        (* x x x));

(define (pow1 x y); x to the yth power(y must be nonnegative)
    (if (= y 0)
        1
        (* x (pow1 x (- y 1)))))

(pow1 3 2)
(pow1 3 0)

(define pow2
    (lambda (x)
        (lambda (y)
            (pow1 x y))))

(define three-to-the (pow2 3))

(three-to-the 2)
(define sixteen (pow1 4 2))
sixteen
(define sixteen1 ((pow2 4) 2))
sixteen1