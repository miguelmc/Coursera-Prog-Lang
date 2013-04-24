
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;problem 1
(define (sequence low high stride)
 (if (> low high)
  null
  (cons low (sequence (+ low stride) high stride))))

;problem 2
(define (string-append-map xs suffix)
 (map (lambda (s) (string-append s suffix)) xs))

;problem 3
(define (list-nth-mod xs n)
 (cond [(< n 0) (error "list-nth-mod: negative number")]
       [(null? xs) (error "list-nth-mod: empty list")]
       [#t 
        (car (list-tail xs (remainder n (length xs))))]))


;problem 4
(define (stream-for-n-steps s n)
 (if (= n 0) 
  null
  (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;problem 5
(define funny-number-stream 
 (letrec ([f (lambda (x) 
              (cons 
               (if (= (remainder x 5) 0) 
                (* x -1)
                x)
               (lambda () (f (+ x 1)))))])
  (lambda () (f 1))))

;problem 6
(define dan-then-dog
 (letrec (
          [f (lambda (x)
               (cons x (lambda () (if (string=? x "dan.jpg")
                                   (f "dog.jpg")
                                   (f "dan.jpg")
                                   ))))])
   (lambda () (f "dan.jpg"))))

;problem 7
;this is getting confusing...
(define (stream-add-zero s)
 (letrec(
         [f (lambda (v)
             (cons (cons 0 (car (v))) 
                   (lambda () (f (cdr (v))))
              ))])
 (lambda () (f s))))

;(define test7 (stream-for-n-steps (stream-add-zero dan-then-dog) 5))

;problem 8
(define (cycle-lists xs ys)
 (letrec(
         [f (lambda (n)
             (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
              (lambda () (f (+ n 1)))))])
  (lambda () (f 0))))

;(define test8 (car((cycle-lists (list 1 2 3) (list "a" "b")))))
;(define test8-2 (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 10))

;problem 9
(define (vector-assoc v vec)
 (letrec(
         [f (lambda (x)
             (if (= x (vector-length vec))
              #f
              (if (pair? (vector-ref vec x))
                (if (equal? (car(vector-ref vec x)) v)
                  (vector-ref vec x)
                  (f (+ x 1)))
                (f (+ x 1)))))])
  (f 0)))

;tests prob 9
(define vec (vector (cons "gola" 1) (cons "aaa" 2) (cons "pedo" 4) (cons "jessy" 6) (cons 6 7)))
(define test9 (vector-assoc 6 vec))

;problem 10
(define (cached-assoc xs n)
 (lambda (v) (assoc v xs)))
