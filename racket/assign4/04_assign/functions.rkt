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

(define (stream-addition stream1 stream2)
 (cons (+ (car(stream1)) (car(stream2))) (lambda () (stream-addition (cdr(stream1)) (cdr(stream2))))))

;#5
(define fibo-stream
  (lambda() (cons 0(lambda()(cons 1 (lambda() (stream-addition  (cdr(fibo-stream)) fibo-stream)))))))

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
(define-syntax create-stream
  (syntax-rules (define letrec lambda cons +)
    [(create-stream name using f starting at i0 with increment delta)
    (define name
      (letrec
      ([fun (lambda (x)
            (cons (f x) (lambda () (fun (+ x delta)))))])
    (lambda () (fun i0)))
    )]))

; part 2

;#1
(define (vector-assoc v vec)
  (letrec ((f (lambda (index)
    (if (= index (vector-length vec))
      #f
      (if (pair? (vector-ref vec index))
        (if (equal? v (car(vector-ref vec index)))
          (vector-ref vec index)
          (f (+ index 1)))
        (f (+ index 1)))))))
  (f 0)))


;#2
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define cache-slot 0)
  (define (cache-mutate v)
    (if (= 0 n)
      null
      (vector-set! cache (modulo cache-slot n) v))
    (set! cache-slot (+ cache-slot 1))
    v)
  (lambda (v)
    (let ((found (vector-assoc v cache)))
    (if (pair? found)
        found
        (let ((assoced (assoc v xs)))
        (if (pair? assoced)
            (cache-mutate assoced)
            #f))))))