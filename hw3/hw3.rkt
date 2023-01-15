#lang errortrace racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions ;;

(define p:empty (delay empty))
(define (p:empty? p) (empty? (force p)))
(define (p:first l) (car (force l)))
(define (p:rest l) (cdr (force l)))
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 1
(define p:void (delay '()))


;; Exercise 2
(define p:epsilon (delay (cons "" p:void)))

;; Exercise 3
(define (p:char p) (delay (cons (string p) p:void)))

;; Exercise 4
(define (p:union p1 p2) (delay (if (p:empty? p1) (force p2) (cons (p:first p1) (p:union p2 (p:rest p1))))))

;; Exercise 5
(define (p:prefix s p) (delay (if (p:empty? p) empty (cons (string-append s (p:first p)) (p:prefix s (p:rest p))))))

;; Exercise 6
(define (p:cat p1 p2) (delay (if (p:empty? p1) empty (force (p:union (p:prefix (p:first p1) p2) (p:cat (p:rest p1) p2))))))


;; Exercise 7

(define (p:star union pow p)
 (let loop ((n 0))
   (p:union 
     (pow p n) 
     (delay (force (loop (+ n 1)))))))

;; Exercse 8
(define (stream-foldl f a s) 
  (cons a (lambda () (stream-foldl f (f (stream-get s) a) (stream-next s)))))

;; Exercise 9
(define (stream-skip n s) (if (zero? n) s (stream-skip (- n 1) (stream-next s))))

;; Exercise 10
(struct r:bool (value))

(define (r:eval-builtin sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [else #f]))

(define (r:eval-exp exp)
  (cond
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones
    [(r:bool? exp) (r:bool-value exp)]
    [(r:apply? exp)
     (cond ((equal? (r:variable 'and) (r:apply-func exp))
            (let loop ((a (r:apply-args exp)) (v (r:bool #t)))
              (cond ((null? a) (r:eval-exp v))
                    ((not (r:eval-exp v)) #f)
                    (#t (loop (cdr a) (car a))))))
           (#t 
             (apply
               (r:eval-exp (r:apply-func exp))
               (map r:eval-exp (r:apply-args exp)))))]
    [else (error "Unknown expression:" exp)]))
