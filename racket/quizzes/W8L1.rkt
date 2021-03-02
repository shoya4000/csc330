#lang racket

(define (repeat n th)
    (th)
    (if (= n 0)
        #t
        (repeat (- n 1) th)))

(repeat 5 (lambda () (print "hello")))