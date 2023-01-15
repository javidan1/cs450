#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

  We ask that solutions be distributed only locally -- on paper, on a
  password-protected webpage, etc.

  Students are required to adhere to the University Policy on Academic
  Standards and Cheating, to the University Statement on Plagiarism and the
  Documentation of Written Work, and to the Code of Student Conduct as
  delineated in the catalog of Undergraduate Programs. The Code is available
  online at: http://www.umb.edu/life_on_campus/policies/code/

                    * * * ATTENTION! * * *

  Every solution submitted to our grading server is automatically compared
  against a solution database for plagiarism, which includes every solution
  from every student in past semesters.

  WE FOLLOW A ZERO-TOLERANCE POLICY: any student breaking the Code of Student
  Conduct will get an F in this course and will be reported according to
  Section II Academic Dishonesty Procedures.

|#

;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1 (/
             (+ 13
                (+ 11 10))
             (- 11
                (+ 10 13))))

(define ex2
  (list
   (/ (+ 13 (+ 11 10))(- 11 (+ 10 13)))
   (/ (+ 13 21) (- 11 (+ 10 13)))
   (/ 34 (- 11 (+ 10 13)))
   (/ 34 (- 11 23))
   (/ 34 -12)
    -17/6))

; (define ex3 (lambda (x y) (< (+ (* 11 y) (- 6 9)) (+ (- 14 y) x))))
(define(ex3 x y)
  ; (11 * y) + (6 - 9) < (14 - y) + x
  (<
      (+ (* 11 y)
         (- 6 9))
      (+ (- 14 y)
         x)
   ))

;; Constructs a tree from two trees and a value
(define (tree left value right) (list left value right))
;; Constructs a tree with a single node
(define (tree-leaf value) (list null value null))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value) (tree (tree-left self) value (tree-right self)))
(define (tree-set-left self left) (tree left (tree-value self) (tree-right self)))
(define (tree-set-right self right) (tree (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value) (cond
   ((null? self) (tree-leaf value))
   ((= (tree-value self) value) (tree-set-value self value))
   ((< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value)))
   (#t (tree-set-right self (bst-insert (tree-right self) value)))))

;; lambda
(define (lambda? node) (and
                        (list? node)
                        (>= (length node) 3)
                        (eq? 'lambda (first node))
                        (list? (second node))
                        (andmap symbol? (second node))))
(define (lambda-params node) (second node))
(define (lambda-body node) (cddr node))

;; apply
(define (apply? l) (and
                    (list? l)
                    (not (null? l))
                    (or (symbol? (first l))
                        (lambda? (first l))
                        (apply? (first l)))
                    (not (lambda? l))
                    (not (define? l))))

(define (apply-func node) (first node))
(define (apply-args node) (cdr node))

;; define
(define (define? node) (or (define-basic? node) (define-func? node)))
(define (define-basic? node) (and
                        (list? node)
                        (= (length node) 3)
                        (eq? 'define (first node))
                        (symbol? (second node))))
(define (define-func? node) (and
                        (list? node)
                        (>= (length node) 3)
                        (eq? 'define (first node))
                        (list? (second node))
                        (not (null? (second node)))
                        (andmap symbol? (second node))))
