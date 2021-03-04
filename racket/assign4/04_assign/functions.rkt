#lang racket


(provide (all-defined-out)) ;; so we can put tests in a second file

; part 1
(define nat-num-stream
  (letrec
      ([f (lambda (x)
            (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;#1
(define (add-pointwise lst1 lst2)
  (if (and (list? lst1) (list? lst2))
    (if (null? lst2)
      lst1 
      (if (null? lst1)
        lst2
        (cons (+ (car lst1) (car lst2)) (add-pointwise (cdr lst1) (cdr lst2)) )))
    (error "illegal parameter")))
  

;#2
(define (add-pointwise-lists lstOflsts)
  (if (list? lstOflsts)
    (if (null? lstOflsts)
      null
      (add-pointwise (car lstOflsts) (add-pointwise-lists (cdr lstOflsts)) ))
    (error "illegal parameter")))

;#3
(define (add-pointwise-lists-2 lstOflsts)
  (if (list? lstOflsts)
    (if (null? lstOflsts)
      null
      (foldl add-pointwise '() lstOflsts ))
    (error "illegal parameter")))
;#4
(define (stream-for-n-steps s n)
  (letrec ((f (lambda (s ans)
                (let ((pr (s)))
                  (if (= ans n)
                    null
                    (cons (car pr) (f (cdr pr) (+ ans 1)) ))))))
    (f s 0)))

;#5
(define fibo-stream
  (letrec
      ([f (lambda (x)
            (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

  (cons 1 (cons 1 (stream-add fibo-stream (stream-rest fibo-stream))))

  (stream-cons 1 (stream-cons 1 (stream-add fibonacci (stream-rest fibonacci)))))
;  (letrec
;      ([f (lambda (x)
;            (cons x (lambda () (f (+ x 1)))))])
;    (lambda () (f 0))))


;#6
(define (filter-stream tester s)
  (letrec
      ([f (lambda (x rest)
            (if (tester x)
              (cons x (lambda () (f (car(rest)) (cdr(rest)))))
              (f (car(rest)) (cdr(rest)))
            ))])
    (lambda () (f (car (s)) (cdr(s))))))

;#7
(define palyndromic-numbers
  (filter-stream (lambda (i) (equal? (string->list(number->string i)) (reverse (string->list(number->string i))))) nat-num-stream))

;#8 macro create-stream

; part 2

;#1
(define vector-assoc #f)

;#2
(define cached-assoc #f)
