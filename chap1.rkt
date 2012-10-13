#lang racket

;;; Ex 1.1

10
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(define a 3)
; #<void>

(define b (+ a 1))
; #<void>

(+ a b (* a b))
; 19

(= a b)
; #f

(if (and (> b a) (< b (* a b)))
    b
    a)
; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; 16

(+ 2 (if (> b a) b a))
; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; 16


;;; Ex 1.2
(/ (+ 5 
      4 
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 
      (- 6 2) 
      (- 2 7)))
; -37/150


;;; Ex 1.3
(define (square n)
  (* n n))

(define (sum-square a b)
  (+ (square a) (square b)))

(define (sum-square-largest a b c)
  (cond ((and (< c a) (< c b)) (sum-square a b))
        ((and (< a b) (< a c)) (sum-square b c))
        (else (sum-square c a))))

(define (sum-square-largest a b c)
  (if (> a b)
      (sum-square a (if (> b c) b c))
      (sum-square b (if (> a c) a c))))
; ^ better than the cond version, compact, less branching

; even better composed version (credits billthelizard) -
(define (max x y)
  (if (> x y) x y))
(define (min x y)
  (if (< x y) x y))

(define (sum-of-highest-squares x y z)
  (+ (square (max x y))
     (square (max (min x y) z))))
; ^ sexier - retains indication of summing in its definition, 
;   logical, has no branching, composition finesse


;; Ex 1.4

; (define (a-plus-abs-b a b)
;   ((if (> b 0) + -) a b))

; substitution model would first evaluate the if special form
; resulting in a new list with + or - as operator, which then gets
; evaluated further by application of operator on a, b


;; Ex 1.5 

; (define (p) (p))
; (define (test x y)
;    (if (- x 0) 0 y))

; Racket traps into infinite loop, which means it tries to evaluate
; arguments first, and then substitutes them into expansions of
; function definitions. Had it been normal order, the if special form
; would nicely short-circuit resulting in a 0 without problems


