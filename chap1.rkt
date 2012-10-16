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


;; Ex. 1.9

; (define (+ a b)
;   (if (= a 0) b (inc (+ (dec a) b))))

; for (+ 4 5), this generates -
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

; this definition of + is linear-recursive, characterized by deferred
; computations, resulting in expansion-compaction

; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))

; by looking at it, this would generate a process of which, the state
; at any point of time can be determined by two variables alone,
; making it linear-iterative process


;; Ex. 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
; 1024

(A 2 4)
; 65536

(A 3 3)
; 65536

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

; by observation -

; (f 0) = 0, (f 1) = 2, (f 2) = 4, (f 3) = 6 and so on
; thus, f = 2n

; (g 0) = 0, (g 1) = 2, (g 2) = 4, (g 3) = 8, (g 4) = 16, (g 5) = 32 and so on
; thus, g = 2^n

; (h 0) = 0, (h 1) = 2, (h 2) = 4, (h 3) = 16, (h 4) = 65536, (h 5) = huge, ...

; 0  0     = 0     = 0
; 1  2     = 2^1   = 2^(2^0) = 2^ h(0)
; 2  4     = 2^2   = 2^(2^1) = 2^ h(1)
; 3  16    = 2^4   = 2^(2^2) = 2^ h(2)
; 4  65536 = 2^16  = 2^(2^4) = 2^ h(3)

; h(n) = { 0         when n = 0
;          2^h(n-1)  when n > 0


;; Ex. 1.11

; recursive process 
(define (fr n)
  (if (< n 3) n
      (+ (fr (- n 1))
         (* 2 (fr (- n 2)))
         (* 3 (fr (- n 3))))))

(fr 0)
; 0
(fr 1)
; 1
(fr 2)
; 2
(fr 3)
; 4
(fr 4)
; 11
(fr 5)
; 25

; iterative process -
; base cases 0:0 1:1 2:2
(define (fi n)
  (define (f-iter a b c count)
    (if (= count 0)
        c
        (f-iter (+ a (* b 2) (* c 3)) a b (- count 1))))
  (f-iter 2 1 0 n))

(fi 0)
; 0
(fi 1)
; 1
(fi 2)
; 2
(fi 3)
; 4
(fi 4)
; 11
(fi 5)
; 25


;; Ex. 1.12

(define (pascal row col)
  (cond ((< row col) #f)      ;; better check this illegal case or else
        ;; interpreters can go on and eat all the ram available
        ((or (= col 0) (= row col)) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(pascal 0 0)
; 1
(pascal 1 0)
; 1
(pascal 0 1)
; #f
(pascal 4 2)
; 6
(pascal 4 1)
; 4
(pascal 4 3)
; 4


; Ex. 1.13


; Ex. 1.14 - using racket's trace, no time to do elaborate ascii art

(define (count-change amount)
   (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)
                 (cc amount
                     (- kinds-of-coins 1))))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(require racket/trace)
(trace cc)
(count-change 11)

;; >(cc 11 5)
;; > (cc -39 5)
;; < 0
;; > (cc 11 4)
;; > >(cc -14 4)
;; < <0
;; > >(cc 11 3)
;; > > (cc 1 3)
;; > > >(cc -9 3)
;; < < <0
;; > > >(cc 1 2)
;; > > > (cc -4 2)
;; < < < 0
;; > > > (cc 1 1)
;; > > > >(cc 0 1)
;; < < < <1
;; > > > >(cc 1 0)
;; < < < <0
;; < < < 1
;; < < <1
;; < < 1
;; > > (cc 11 2)
;; > > >(cc 6 2)
;; > > > (cc 1 2)
;; > > > >(cc -4 2)
;; < < < <0
;; > > > >(cc 1 1)
;; > > > > (cc 0 1)
;; < < < < 1
;; > > > > (cc 1 0)
;; < < < < 0
;; < < < <1
;; < < < 1
;; > > > (cc 6 1)
;; > > > >(cc 5 1)
;; > > > > (cc 4 1)
;; > > > > >(cc 3 1)
;; > > > > > (cc 2 1)
;; > > > >[10] (cc 1 1)
;; > > > >[11] (cc 0 1)
;; < < < <[11] 1
;; > > > >[11] (cc 1 0)
;; < < < <[11] 0
;; < < < <[10] 1
;; > > > >[10] (cc 2 0)
;; < < < <[10] 0
;; < < < < < 1
;; > > > > > (cc 3 0)
;; < < < < < 0
;; < < < < <1
;; > > > > >(cc 4 0)
;; < < < < <0
;; < < < < 1
;; > > > > (cc 5 0)
;; < < < < 0
;; < < < <1
;; > > > >(cc 6 0)
;; < < < <0
;; < < < 1
;; < < <2
;; > > >(cc 11 1)
;; > > > (cc 10 1)
;; > > > >(cc 9 1)
;; > > > > (cc 8 1)
;; > > > > >(cc 7 1)
;; > > > > > (cc 6 1)
;; > > > >[10] (cc 5 1)
;; > > > >[11] (cc 4 1)
;; > > > >[12] (cc 3 1)
;; > > > >[13] (cc 2 1)
;; > > > >[14] (cc 1 1)
;; > > > >[15] (cc 0 1)
;; < < < <[15] 1
;; > > > >[15] (cc 1 0)
;; < < < <[15] 0
;; < < < <[14] 1
;; > > > >[14] (cc 2 0)
;; < < < <[14] 0
;; < < < <[13] 1
;; > > > >[13] (cc 3 0)
;; < < < <[13] 0
;; < < < <[12] 1
;; > > > >[12] (cc 4 0)
;; < < < <[12] 0
;; < < < <[11] 1
;; > > > >[11] (cc 5 0)
;; < < < <[11] 0
;; < < < <[10] 1
;; > > > >[10] (cc 6 0)
;; < < < <[10] 0
;; < < < < < 1
;; > > > > > (cc 7 0)
;; < < < < < 0
;; < < < < <1
;; > > > > >(cc 8 0)
;; < < < < <0
;; < < < < 1
;; > > > > (cc 9 0)
;; < < < < 0
;; < < < <1
;; > > > >(cc 10 0)
;; < < < <0
;; < < < 1
;; > > > (cc 11 0)
;; < < < 0
;; < < <1
;; < < 3
;; < <4
;; < 4
;; <4
;; 4


;; Ex 1.15

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(trace p)

(sine 12.5)

; a) by trace, p is applied 5 times

;; >(p 0.051440329218107005)
;; <0.153776521096694
;; >(p 0.153776521096694)
;; <0.4467840153484446
;; >(p 0.4467840153484446)
;; <0.9836111719853284
;; >(p 0.9836111719853284)
;; <-0.8557060643295382
;; >(p -0.8557060643295382)
;; <-0.060813768577286265
;; -0.060813768577286265

; b) obsevations with tracing p - 
;    for a in 3..9     -> 4 calls to p
;    for a in 9..15    -> 5 calls to p
;    for a in 15..27   -> 6 calls to p

;    for a change of 3 times in angle, num calls to p changes by 1
;    number of steps - log(n) nature
;    growth in space - p needs to be accumulated - p varies as log(n)
;    hence space too varies logarithmically


;; Ex. 1.16

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-exp b n)
  (fe-iter b n 1))

(define (fe-iter b n p)
  (cond ((= n 0) p)
        ((even? n) (fe-iter (square b) (/ n 2) p))
        (else (fe-iter b (- n 1) (* p b)))))

(fast-exp 2 3)
; 8
(fast-exp 3 0)
; 1
(fast-exp 4 4)
; 256


;; Ex. 1.17

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (mul a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))

(mul 3 8)
; 24
(mul 8 1)
; 8
(mul 8 0)
; 0
(mul 0 6)
; 0

; is logarithmic in b, for multiplying with 2b, we need b steps.


;; Ex. 1.18

(define (mul-i a b)
  (mul-iter a b 0))

(define (mul-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (mul-iter (double a) (halve b) p))
        (else (mul-iter a (- b 1) (+ p a)))))

(mul-i 3 8)
; 24
(mul-i 8 1)
; 8
(mul-i 8 0)
; 0
(mul-i 0 6)
; 0


;; Ex. 1.19

; Taking general case, Tpq transforms as -
;   a <- bq + ap + aq
;   b <- bp + aq

; Applying Tpq over itself -
; For a -
;     a <- (bp+aq)q + (p+q)(bp + ap + aq)
;       <- bpq + aq^2 + bpq + ap^2 + apq + bq^2 + apq + aq^2
;       <- 2bpq + 2aq^2 + 2apq + ap^2 + bq^2
;       <- b(2pq + q^2) + a(2q^2 + 2pq + p^2)
;
; For b -
;     b <- (bp + aq)p + (bq + ap + aq)q
;       <- bp^2 + apq + bq^2 + apq + aq^2
;       <- 2apq + aq^2 + bp^2 + bq^2
;       <- a(2pq + q^2) + b(p^2 + q^2)

; Tp'q' would bring the following transform (grouping a,b) -
; For a - 
;     a <- bq' + a(q' + p')
;
; For b -
;     b <- bp' + aq'

; Comparing with (Tpq)^2 transforms -
; From coefficients of a -
;    q' = 2pq + q^2                           [1]
;    q' + p' = 2q^2 + 2pq + p^2               [2]
;
; From coefficients of b -
;    p' = p^2 + q^2                           [3]
;    q' = 2pq + q^2                           [4]

; adding [3] [4] confirms [2], thus testing with our definition -

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))               ; computed p'
                   (+ (* 2 p q) (square q))                ; computed q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 0)
; 0
(fib 1)
; 1
(fib 2)
; 1
(fib 3)
; 2
(fib 4)
; 3
(fib 5)
; 5

; this is fibonacci indeed, having found (Tpq)^2 in terms of
; parameters p,q themselves, we're able to compute Fib(n)
; logarithmically iteratively



