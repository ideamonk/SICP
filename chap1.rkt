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

;(define (sum-square-largest a b c)
;  (cond ((and (< c a) (< c b)) (sum-square a b))
;        ((and (< a b) (< a c)) (sum-square b c))
;        (else (sum-square c a))))

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

; Racket traps into an infinite loop, which means it tries to evaluate
; arguments first, and then substitutes them into expansions of
; function definitions, hence it is applicative order. Had it been 
; normal order, the if special form would nicely short-circuit
; resulting in a 0 without problems


;; Ex 1.6

(define (average x y)
  (/ (+ x y) 2))

(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       0.001))
  (define (improve guess)
    (average guess
             (/ x guess)))
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  (try 1.0))

; from 1.5, we know that racket is applicative order. If we use
; new-if, racket would treat it as a normal function, it will
; evaluate the argument, i.e. (try (improve guess)) first, which would
; cause an infinite loop due to try's recursive definition.


;; Ex 1.7 

(sqrt 0.01)
; 0.10032578510960605
(sqrt 0.001)
; 0.04124542607499115
(sqrt 0.0001)
; 0.03230844833048122
(sqrt 0.00001)
; 0.03135649010771716
; fixates inaccurately around 0.032... as we diminish x further

; not very effective for very small numbers - 
; as we approach 0.001, good-enough? becomes less truthy and
; ineffective, as things fall within the tolerance of 0.001

; inadequate for very large numbers -

 (sqrt 17640000000000)
; 4200000.0

;(sqrt 176400000000000)
; traps

 (sqrt 1764000000000000)
; 42000000.0

; sqrt evaluates nicely for square numbers, even if very large
; but the second invocation traps into an infinite loop

; symbol guess's values follow the following pattern -
; 1.0
; 88200000000000.5
; 44100000000001.25
; ...
; 15589740.92200749
; 13452437.218607213
; 13282651.363214033
; 13281566.21703714
; 13281566.172707193
; ^ saturates to this forever

; three things happen here -

; 1) x/guess turns out to be 13281566.172707194 - very close to
; 13281566.172707193

; 2) average of guess and x/guess turns out to be guess again -
; 13281566.172707193

; 3) when good-enough? squares the guess, by limitations of floating
; point calculations, the result is 176399999999999.97, which is never
; good enough because the difference (0.03125) exceeds tolerance of 0.001

(define (sqrt-better x)
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess))
       0.001))
  (define (improve guess)
    (average guess
             (/ x guess)))
  (define (try guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (try (improve guess) guess)))
  (try 1.0 -1))

; sqrt-better keeps track of previous guess, returns when the guess
; saturates

(sqrt-better 176400000000000)
;13281566.172707193


;; Ex. 1.8

(define (cube x)
  (* x x x))

(define (cube-root x)
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess))
       0.001))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))
  (define (try guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (try (improve guess) guess)))
  (try 1.0 -1))

(cube-root 27)
;3.0000000000000977


