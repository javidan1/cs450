#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1.a: Counter
(define (counter accum grow)
  (lambda (symbol)
    (cond
      [(equal? symbol 'inc)
       (counter (grow accum) grow)]
      [(equal? symbol 'get)
       accum]
      [else
       (r:void)])))

;; Exercise 1.b: Adder
(define (adder super)
  (lambda (symbol)
    (cond
      [(equal? symbol 'inc)
       (adder (super 'inc) (super 'inc))]
      [(equal? symbol 'get)
       (super 'get)]
      [else
       (r:void)])))

;; Exercise 2: Interperse
(define (intersperse l e)
  (if (null? l)
      empty
      (cons (car l)
            (cons e
                  (intersperse (cdr l) e)))))

;; Exercise 3.a: Generic find
(define (find pred l)
  (define (find-helper accum l)
    (cond [(null? l) #f]
          [(pred accum (car l)) (cons accum (car l))]
          [else (find-helper (+ 1 accum) (cdr l))]))
  (find-helper 0 l))

;; Exercise 3.b: Member using find
(define (member x l)
  (define (is-member n e)
    (and (equal? x e)
         n))
  (find is-member l))

;; Exercise 3.c: index-of using find
(define (index-of l x)
  (let ((result (find (lambda (idx elem) (equal? elem x)) l)))
    (if (pair? result)
        (car result)
        #f)))

;; Exercise 4: uncurry
(define (uncurry f)
  (lambda (l)
    (define (uncurry-start l accum)
      (cond [(null? l) accum]
            [(= (length l) 1) (accum (car l))]
            [else (uncurry-start (cdr l) (accum (car l)))]))
    (uncurry-start l f)))

;; Exercise 5: Parse a quoted AST
(define (parse-ast node)
  (define (make-define-func node)
    (r:define (make-variable (first (second node)))
              (r:lambda (map make-variable (rest (second node)))
                        (map parse-ast (rest (rest node))))))

  (define (make-define-basic node)
    (r:define (make-variable (second node))
              (parse-ast (first (rest (rest node))))))

  (define (make-lambda node)
    (r:lambda (map make-variable (second node))
              (map parse-ast (rest (rest node)))))

  (define (make-apply node)
    (r:apply (parse-ast (first node))
             (map parse-ast (rest node))))

  (define (make-number node)
    (r:number node))

  (define (make-variable node)
    (r:variable node))

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
