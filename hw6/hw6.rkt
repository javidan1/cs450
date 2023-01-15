#lang errortrace racket
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
(require racket/set)
(require "hw6-util.rkt")
(provide frame-refs mem-mark mem-sweep mlist mapply)
;; END OF REQUIRES

;;;;;;;;;;;;;;;
;; Exercise 1
;
; Given a frame, return all handles contained therein.
;
; Use frame-fold and set-add; only closure's env is relevant for this problem
(define/contract (frame-refs frm)
(-> frame? set?)
(define result (set))
(define (add-handle h)
(set-add h result))
(frame-fold
(lambda (k v s)
(cond
[(handle? v) (add-handle v)]
[(d:closure? v) (add-handle (d:closure-env v))]))
result
frm)
result)
;;;;;;;;;;;;;;;
;; Exercise 2
; Standard graph algorithm: return all handles reacheable via `contained`.
; Hint: This is a simple breadth-first algorith. The algorithm should start
;       in env and obtain the set of next elemens by using `contained`.
; Hint: The algorithm must handle cycles.
; Hint: Do not hardcode your solution to frames (you should test it with
;       frames though)
(define/contract (mem-mark contained mem env)
(-> (-> any/c set?) heap? handle? set?)
(define marked (set))
(define (mem-mark-helper to-visit visited)
(cond
[(empty? to-visit) marked]
[else
(define current (first to-visit))
(if (set-member? current visited)
(mem-mark-helper (rest to-visit) visited)
(begin
(set marked (set-add current marked))
(mem-mark-helper (append (rest to-visit) (contained (heap-get mem current)))
(set-add current visited))))]))
(mem-mark-helper (list env) (set)))


;;;;;;;;;;;;;;;
;; Exercise 3
;
; Return a new heap that only contains the key-values referenced in to-keep.
;
; Tip 1: We have learned a similar pattern than what is asked here.
; Tip 2: The function you want to use starts with heap-
; Tip 3: The solution is a one-liner

(define/contract (mem-sweep mem to-keep)
  (-> heap? set? heap?)
  (heap-filter (lambda (key val) (set-member? to-keep key))mem))

;;;;;;;;;;;;;;;
;; Exercise 4

; Recursively evaluate each argument and use bind to sequence the recursion step
;
; x1 <- op1
; x2 <- op2
; x3 <- op3
;
; Should become:
;
; l <- (list op1 op2 op3)
;
; Tip: The solution of this example is quite similar to the example in
;      yield to-list (video 6 of lecture 32)

(define (mlist bind pure args)
  (define (mlist-helper accum lst)
    (cond [(empty? lst) (pure accum)]
          [else (bind (first lst)                      (lambda (x) (mlist-helper (append accum (list x)) (rest lst))))]))
  (mlist-helper '() args))


;;;;;;;;;;;;;;;
;; Exercise 5

; Can be solved with one line if you use mlist, apply, and compose.
;
(define (mapply bind pure f . args)
  (define (mlist ops)
    (if (null? ops)
        (pure '())
        (bind (car ops)
              (lambda (x)
                (bind (mlist (cdr ops))
                      (lambda (xs)
                        (pure (cons x xs))))))))
  (bind (mlist args)
        (lambda (xs)
          (pure (apply f xs)))))

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|

If the reference count overflows and resets to zero, it may
pose issues with the soundness and completeness of memory management.
Overflowing the reference count will cause the reference count to become inaccurate.
If the reference count is not correct, it might signal that an object is being utilized
while it not. It means that the object will not be correctly deallocated and result in a memory leak.

|#
