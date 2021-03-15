#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

;; CHANGE (put your solutions here)
(define (mupllist->racketlist lst)
  (if (aunit? lst)
      null
      (if (apair? lst)
          (cons (mupllist->racketlist (apair-e1 lst)) (mupllist->racketlist (apair-e2 lst)))
          lst)))
      
(define (racketlist->mupllist lst)
 (if (null? lst)
      (aunit)
      (if (list? (car lst))
          (apair (racketlist->mupllist (car lst)) (racketlist->mupllist (cdr lst)))
          (apair (car lst) (racketlist->mupllist (cdr lst))))))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp (see below).
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; "CHANGE" add more cases here
        ;; one for each type of expression
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               )]
        [(aunit? e) e]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
         (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL fst applied to non-pair")))]
        [(isaunit? e)
          (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(mlet? e)
         (let ([env (cons (cons (mlet-var e)(eval-under-env (mlet-e e) env))env )])
              (eval-under-env (mlet-body e) env))]
        [(fun? e)
         (closure env e)]
        [(closure? e)
         (eval-under-env (closure-fun e) (closure-env e))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let ([env (cons (cons (fun-formal (closure-fun v1)) v2) (closure-env v1))])
                 (if (fun-nameopt (closure-fun v1))
                     (let ([env (cons (cons (fun-nameopt (closure-fun v1)) v1) env)])
                       (eval-under-env (fun-body (closure-fun v1)) env))
                     (eval-under-env (fun-body (closure-fun v1)) env)))  
               (error "MUPL call applied to a non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
;; note how evaluating an expression start with an empty environment
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) (if (aunit? e1) e2 e3))

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem D

(define mupl-map "CHANGE")
;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;; 

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))
