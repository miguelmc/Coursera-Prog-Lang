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

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;a)
(define (racketlist->mupllist rlist)
 (if (null? rlist)
  (aunit)
  (cons (car rlist) (racketlist->mupllist (cdr rlist)))))

;b
(define (mupllist->racketlist mlist)
 (if (aunit? mlist)
  null
  (cons (car mlist) (mupllist->racketlist (cdr mlist)))))

;; Problem 2

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

;it (worked?) somehow; super mindbending. won't do this again.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(int? e)
         e]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (int (+ (int-num v1)
                     (int-num v2)))
             (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
             (error "MUPL addition applied to non-number")))]
        [(apair? e)
         (cons (eval-under-env (apair-e1) env) (eval-under-env (apair-e2) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? e)
             (eval-under-env (apair-e1 v) env)
             (error "Expression is not a pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? e)
             (eval-under-env (apair-e2 v) env)
             (error "Expression is not a pair")))]
        ;; CHANGE add more cases here
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (let ([newEnv (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env)])
          (eval-under-env (mlet-body e) newEnv ))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)])
          (if (closure? c)
           (let([f (closure-fun c)]
                [newEnv (cons 
                         (cons (fun-formal (closure-fun c)) 
                          (eval-under-env (call-actual e) env))
                         (closure-env c))])
             (if (fun-nameopt f)
               (eval-under-env (fun-body f)
                (cons (cons (fun-nameopt f) c) newEnv))
               (eval-under-env (fun-body f) newEnv)))
           (error "Not a closure")))]
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) 
 (ifgreater (isaunit e1) (int 0) e2 e3))
    
(define (mlet* lstlst e2) 
  (if (null? lstlst)
    e2
    (mlet (caar lstlst) (cdar lstlst) 
     (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "v1" e1) 
          (cons "v2" e2))
         (ifgreater (var "v1") (var "v2") e4
                    (ifgreater (var "v2") (var "v1") 
                     e4 e3))))

;; Problem 4

(define mupl-map
    (fun #f "map"
         (fun "new-map" "l" (ifaunit (var "l")
                       (aunit)
                       (apair 
                        (call (var "map") (fst (var "l")))
                              (call (var "new-map") 
                               (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "x"
             (call (var "map")
                   (fun #f "y" 
                    (add (var "x") (var "y")))))))



;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
