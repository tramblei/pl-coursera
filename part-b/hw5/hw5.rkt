;; Programming Languages, Homework 5

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

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

;; Problem 2

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))
      

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
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
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)]
               )
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)]
               [var (mlet-var e)])
           (eval-under-env (mlet-body e) (cons (cons var v) env)))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([cl (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cl)
               (let* ([f (closure-fun cl)]
                      [fun-name (fun-nameopt f)]
                      [arg-name (fun-formal f)]
                      [fun-body (fun-body f)]
                      [new-env (cons (cons arg-name arg) (closure-env cl))]
                      [new-env (if fun-name
                                   (cons (cons fun-name cl) new-env)
                                   new-env)])
                 (eval-under-env fun-body new-env))
               (error "MUPL call applied to non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([binding (car lstlst)])
        (mlet (car binding) (cdr binding) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3)))) ; if _x not greater than _y and _y not greater than _x, then _x and _y must be equal

;; Problem 4

(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "xs"
           (ifeq (isaunit (var "xs")) (int 1)
                 (aunit)
                 (apair (call (var "f") (fst (var "xs"))) (call (call (var "mupl-map") (var "f")) (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(struct res (e fvs) #:transparent) ; e = expression, fvs = free variables

(define (compute-free-vars e)
  (letrec ([f (lambda (e)
                (cond [(var? e) (res e (set (var-string e)))]
                       [(int? e) (res e (set))]
                       [(add? e)
                        (let ([r1 (f (add-e1 e))]
                              [r2 (f (add-e2 e))])
                          (res (add (res-e r1) (res-e r2)) (set-union (res-fvs r1) (res-fvs r2))))]
                       [(ifgreater? e)
                        (let ([r1 (f (ifgreater-e1 e))]
                              [r2 (f (ifgreater-e2 e))]
                              [r3 (f (ifgreater-e3 e))]
                              [r4 (f (ifgreater-e4 e))])
                          (res (ifgreater (res-e r1) (res-e r2) (res-e r3) (res-e r4)) (set-union (res-fvs r1) (res-fvs r2) (res-fvs r3) (res-fvs r4))))]
                       [(aunit? e) (res e (set))]
                       [(apair? e)
                        (let ([r1 (f (apair-e1 e))]
                              [r2 (f (apair-e2 e))])
                          (res (apair (res-e r1) (res-e r2)) (set-union (res-fvs r1) (res-fvs r2))))]
                       [(fst? e)
                        (let ([r (f (fst-e e))])
                          (res (fst (res-e r)) (res-fvs r)))]
                       [(snd? e)
                        (let ([r (f (snd-e e))])
                          (res (snd (res-e r)) (res-fvs r)))]
                       [(isaunit? e)
                        (let ([r (f (isaunit-e e))])
                          (res (isaunit (res-e r)) (res-fvs r)))]
                       [(mlet? e)
                        (let ([r (f (mlet-e e))]
                              [body (f (mlet-body e))])
                          (res (mlet (mlet-var e) (res-e r) (res-e body))
                               (set-union (res-fvs r) (set-remove (res-fvs body) (mlet-var e)))))]
                       [(closure? e)
                        (let ([r (f (closure-fun e))])
                          (res (closure (closure-env e) (res-e r)) (res-fvs r)))]
                       [(call? e)
                        (let ([r1 (f (call-funexp e))]
                              [r2 (f (call-actual e))])
                          (res (call (res-e r1) (res-e r2)) (set-union (res-fvs r1) (res-fvs r2))))]
                       [(fun? e)
                        (let* ([r (f (fun-body e))]
                              [new-fvs (set-remove (res-fvs r) (fun-formal e))]
                              [new-fvs (if (fun-nameopt e)
                                           (set-remove new-fvs (fun-nameopt e))
                                           new-fvs)])
                          (res (fun-challenge (fun-nameopt e) (fun-formal e) (res-e r) new-fvs) new-fvs))]))])
    (res-e (f e))))
                       

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)]
               )
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env-c (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(mlet? e)
         (let ([v (eval-under-env-c (mlet-e e) env)]
               [var (mlet-var e)])
           (eval-under-env-c (mlet-body e) (cons (cons var v) env)))]
        [(fun-challenge? e) (closure
                             (letrec ([s (set->list (fun-challenge-freevars e))]
                                      [f (lambda (xs)
                                        (if (null? xs)
                                            null
                                            (cons (cons (car xs) (envlookup env (car xs))) (f (cdr xs)))))])
                               (f s))
                             e)]
        [(call? e)
         (let ([cl (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? cl)
               (let* ([f (closure-fun cl)]
                      [fun-name (fun-challenge-nameopt f)]
                      [arg-name (fun-challenge-formal f)]
                      [fun-body (fun-challenge-body f)]
                      [new-env (cons (cons arg-name arg) (closure-env cl))]
                      [new-env (if fun-name
                                   (cons (cons fun-name cl) new-env)
                                   new-env)])
                 (eval-under-env-c fun-body new-env))
               (error "MUPL call applied to non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
