#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang errortrace racket
(provide (all-defined-out))
(require "hw4-util.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))

;; Exercise 1
(define (s:subst exp var val) (cond
    [(s:number? exp) exp]
    [(s:variable? exp)
     (if (equal? (s:variable-name exp) (s:variable-name var))
         val
         exp)]
    [(s:lambda? exp)
     (let ([params (s:lambda-params exp)]
           [body (s:lambda-body exp)])
       (s:lambda params (map (lambda (e) (s:subst e var val)) body)))]
    [(s:apply? exp)
     (let ([func (s:apply-func exp)]
           [args (s:apply-args exp)])
       (s:apply (s:subst func var val) (map (lambda (e) (s:subst e var val)) args)))]))

;; Exercise 2
(define (s:eval subst exp)
  (cond
    [(s:number? exp) exp]
    [(s:variable? exp) (subst exp)]
    [(s:lambda? exp) exp]
    [(s:apply? exp)
     (let ([func (s:eval subst (s:apply-func exp))]
           [args (map (λ (e) (s:eval subst e)) (s:apply-args exp))])
       (cond
         [(s:lambda? func)
          (let* ([params (s:lambda-params func)]
                 [body (s:lambda-body func)]
                 [subst-params (map s:variable-name params)]
                 [subst-vals args]
                 [new-subst
                  (λ (x)
                    (let ([x-name (s:variable-name x)])
                      (if (member x-name subst-params)
                          (car (member x-name subst-params subst-vals))
                          (subst x))))])
            (s:eval new-subst (car body)))]
         [else (error 's:eval "not a lambda")]))]))



;; Exercise 3
(define (e:eval env exp)
  (cond
    [(e:value? exp) exp]
    [(and (e:variable? exp) (hash? env) (hash-has-key? env exp))
      (hash-ref env exp)]
    [(and (e:lambda? exp) (hash? env))
      (e:closure env exp)]
    [(e:apply? exp)
      (let*
          [(ef (e:apply-func exp))
           (ef-val (e:eval env ef))
           (ea (e:apply-arg1 exp))
           (ea-val (e:eval env ea))
           (env1 (hash-set (e:closure-env ef-val)
                           (e:lambda-param1 ef-val)
                           ea-val))]
        (e:eval env1 (e:lambda-body ef-val)))]
    [else
     (error "Unknown expression:" exp)]))

;; Exercise 4 (Manually graded)
#|
λ-Racket without environments is the better alternative when we have a small number of variables that do not change value during the course of the program.
λ-Racket with environments is the better alternative when we have lexical scope.
|#

;; Exercise 5 (Manually graded)
#|
The main advantages can be that a formal specification presents is an easy-to-understand version and makes it effortless to understand the behavior of the code.
Secondly, formal specfications can help us develop test cases. Developing test cases is important to ensure that our code satisfies the given requirements.
|#
