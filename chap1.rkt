#lang planet neil/sicp

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

(define (Ak x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Ak (- x 1)
                 (Ak x (- y 1))))))

(Ak 1 10)
; 1024

(Ak 2 4)
; 65536

(Ak 3 3)
; 65536

(define (f n) (Ak 0 n))
(define (g n) (Ak 1 n))
(define (h n) (Ak 2 n))
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

;(require racket/trace)
;(trace cc)
;(count-change 11)

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

;(trace p)

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
; manipulation of parameters p,q themselves, we're able to compute
; Fib(n) logarithmically iteratively, in a way similar to fast-exp

; While exponentiation and multiplication used the idea over numerical
; computations, this example goes ahead to generalize it over a
; transformation


;; Ex. 1.20

; strictly for tracing
(define (rem a b)
  (remainder a b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (rem a b))))

;(trace rem)
(gcd 206 40)

;; >(rem 206 40)
;; <6
;; >(rem 40 6)
;; <4
;; >(rem 6 4)
;; <2
;; >(rem 4 2)
;; <0
;; 2
; Since we know racket is applicative order, by the trace, remainder
; gets 4 calls

; In normal order evaluation - fully expand
; (gcd 206 40)

;; (if (= 40 0) 206
;;     (gcd 40 (remainder 206 40)))

;; (if (= 40 0) 206
;;     (if (= (remainder 206 40) 0) 40
;;         (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))

;; (if (= 40 0) 206
;;     (if (= (remainder 206 40) 0) 40
;;         (if (= (remainder 40 (remainder 206 40)) 0)            ;; is 4
;;             (remainder 206 40)
;;             (gcd (remainder 40 (remainder 206 40)) 
;;                  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

;; (if (= 40 0) 206
;;     (if (= (remainder 206 40) 0) 40
;;         (if (= (remainder 40 (remainder 206 40)) 0)            
;;             (remainder 206 40)
;;             (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ;; is 2
;;                 (remainder 40 (remainder 206 40))
;;                 (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;                      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))))
                                 
;; (if (= 40 0) 206
;;     (if (= (remainder 206 40) 0) 40
;;         (if (= (remainder 40 (remainder 206 40)) 0)            
;;             (remainder 206 40)
;;             (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ;; is 2
;;                 (remainder 40 (remainder 206 40))
;;                 (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ;; is 0
;;                     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;                     (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;;                          (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;                                     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))))))

; at this point, the inner-most if special form's predicate evaluates
; to 0, thus ending further gcd calls. By replacing remainder with our
; rem function, which is set for trace, in the above expression, we
; find a total of 18 remainder calls. We could also do the manual
; labour of *carefully* substituting values into the above expression.
; Careful as in respecting the if special form and not substituting
; every other remainder call for the count.

;; >(rem 206 40)
;; <6
;; >(rem 206 40)
;; <6
;; >(rem 40 6)
;; <4
;; >(rem 206 40)
;; <6
;; >(rem 206 40)
;; <6
;; >(rem 40 6)
;; <4
;; >(rem 6 4)
;; <2
;; >(rem 206 40)
;; <6
;; >(rem 40 6)
;; <4
;; >(rem 206 40)
;; <6
;; >(rem 206 40)
;; <6
;; >(rem 40 6)
;; <4
;; >(rem 6 4)
;; <2
;; >(rem 4 2)
;; <0
;; >(rem 206 40)
;; <6
;; >(rem 206 40)
;; <6
;; >(rem 40 6)
;; <4
;; >(rem 6 4)
;; <2


;; Ex. 1.21

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(smallest-divisor 199)
; 199

(smallest-divisor 1999)
; 1999

(smallest-divisor 19999)
; 7


;; Ex. 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes a b primality-test?)
  (define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))
  (define (start-prime-test n start-time)
    (if (primality-test? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

  (cond ((> a b) #f)
        ((even? a) (search-for-primes (inc a) b primality-test?))
        (else (timed-prime-test a)
              (search-for-primes (+ a 2) b primality-test?))))

; by observation in repl, for first 3 > 1000
(search-for-primes 1000 1020 prime?)
; averages   8.33 ms

; first 3 > 10000
(search-for-primes 10000 10038 prime?)
; averages  31.33 ms

; first 3 > 100000
(search-for-primes 100000 100044 prime?)
; averages  90.00 ms

; first 3 > 1000000
(search-for-primes 1000000 1000038 prime?)
; averages 281.67 ms

; first 3 > 10000000
(search-for-primes 10000000 10000104 prime?)
; averages 829.00 ms

; n             time          ratio
; 1000            8.33          -
; 10000          31.33          3.7611
; 100000         90.00          2.8726
; 1000000       281.67          3.1296
; 10000000      829.00          2.9431
; -------------------------------------
;                     average - 3.1766

; sqrt 10 ~= 3.162277
; indeed the average ratio comes close to sqrt of 10
; although, results vary individually, their average closeness to
; square root of 10 indicates that programs on my machine run in time
; proportional to the number of steps required for computation


; Ex. 1.23

(define (next-n n)
  (if (= n 2) 3
      (+ n 2)))

(define (smallest-divisor-improved n)
  (find-divisor-generic n 2 next-n))

(define (find-divisor-generic n test-divisor next)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-generic n (next test-divisor) next))))

(smallest-divisor-improved 199)
; 199

(smallest-divisor-improved 1999)
; 1999

(smallest-divisor-improved 19999)
; 7

; turned search-for-prime into a more generic function, thus testing
; with prime-improved -

(define (prime-improved? n)
  (= n (smallest-divisor-improved n)))

(search-for-primes 1000 1020 prime-improved?)
; averages   5.33 ms

; first 3 > 10000
(search-for-primes 10000 10038 prime-improved?)
; averages  15.33 ms

; first 3 > 100000
(search-for-primes 100000 100044 prime-improved?)
; averages  48.33 ms

; first 3 > 1000000
(search-for-primes 1000000 1000038 prime-improved?)
; averages 150.33 ms

; first 3 > 10000000
(search-for-primes 10000000 10000104 prime-improved?)
; averages 482.33 ms

; n             time          ratio - prevtime/newtime
; 1000            5.33          1.5628
; 10000          15.33          2.0437
; 100000         48.33          1.8621
; 1000000       150.33          1.8736
; 10000000      482.33          1.7187
; -------------------------------------
; average -                     1.8121

; We get a close to 2x, i.e. ~1.8x improvement, but not 2x as we were
; expecting. In general it tends to 1.8x, this might be due to the
; fact that - we replaced a primitive function + with a branched and
; more complex function - resulting in additional overhead.


;; Ex. 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m ))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fast-prime-tester? n)
  (fast-prime? n 100))

; using fast-prime-tester for measuring -

(search-for-primes 1000 1020 fast-prime-tester?)
; averages  340.33 ms

; first 3 > 10000
(search-for-primes 10000 10038 fast-prime-tester?)
; averages  484.33 ms

; first 3 > 100000
(search-for-primes 100000 100044 fast-prime-tester?)
; averages  587.00 ms

; first 3 > 1000000
(search-for-primes 1000000 1000038 fast-prime-tester?)
; averages  655.00 ms

; first 3 > 10000000
(search-for-primes 10000000 10000104 fast-prime-tester?)
; averages  809.00 ms

; Since we got best performance on prime-improved?, lets compare
; fermat's with that -

; n             time          ratio - fermat/improved
; 1000          340.33          63.85
; 10000         484.33          31.59
; 100000        587.00          12.14
; 1000000       655.00           4.35
; 10000000      809.00           1.67
; -------------------------------------

; fermat's performs poorly for smaller n, compared to improved
; versions. Let's consider fermat's nature by comparing ratios with
; successive calls to itself -

; n             time          ratio - current/prev
; 1000          340.33          -
; 10000         484.33          1.4231
; 100000        587.00          1.2119
; 1000000       655.00          1.1158
; 10000000      809.00          1.2351
; -------------------------------------
; average                       1.2487

; T-1000000 vs T-1000 -> is ~2.37x. Time(steps) grow slowly compared
; to growth in N, indicating approx logarithmic growth


;; Ex. 1.25

; expmod uses breaks the problem into half-size pieces for even cases
; and does that by successive squaring, but on the other hand,
; Alyssa's version would evaluate (fast-exp base exp) first, which
; could cause it to perform poorer in cases of large base,exp values
; in comparison to expmod's working. Thus, she's not correct and this
; wouldn't serve as well for fast prime test.


; Ex. 1.26

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder (* (expmod base (/ exp 2) m)
;;                        (expmod base (/ exp 2) m))
;;                        m))
;;          (else
;;           (remainder (* base (expmod base
;;                                      (- exp 1)
;;                                      m))
;;                      m))))

; One obviously visible difference is that Reasoner's algorithm would
; do two computations of (expmod base (/ exp 2) m) instead of old
; version where once computed, this value was re-used in squaring.
; While with square we were able to reduce problem size by 2 in one
; call, here we do reduce problem size by two, but in turn double the
; number of calls. So, any benefits of halving the problem size to be
; had is cancelled by duplicated computation, in turn taking away the
; logarithmic nature, and turning it into O(n)
; This has repeated computations akin to fibonacci's call graphs.


; Ex. 1.27

;; (define (fermat-test-full n)
;;   (define (try-it a)
;;     (if (= a 1) #t
;;         (and (= (expmod a n n) a) (try-it (- a 1)))))
;;   (try-it (- n 1)))

; This implementation could actually short-circuit on a false result
; instead of evaluating all expmods and then AND-ing the bools over.
; Improved version -

(define (fermat-test-full n)
  (define (try-it a)
    (cond ((= a 1) #t)
          ((not (= (expmod a n n) a)) #f)
          (else (try-it (- a 1)))))
  (try-it (- n 1)))

(fermat-test-full 900)
; #f

(fermat-test-full 561)
; #t

(fermat-test-full 1105)
; #t

(fermat-test-full 1729)
; #t

(fermat-test-full 2465)
; #t

(fermat-test-full 2821)
; #t

(fermat-test-full 6601)
; #t


;; Ex. 1.28

; checks and returns signal via 0 as in hint
(define (non-trivial x n)
  (if (not (or (= x 1)
           (= x (- n 1))
           (not (= (remainder (square x) n) 1))))
      0
      (remainder (square x) n)))

; modified to check for non-trivial roots
(define (expmod-miller base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (non-trivial (expmod-miller base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod-miller base (- exp 1) m))
                    m))))

(define (miller-test n)
  (define (try-it a)
    (= (expmod-miller a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 1)))))

(miller-test 4)
; #f

(miller-test 199)
; #t

(miller-test 1999)
; #t

(miller-test 561)
; #f

(miller-test 6601)
; #f

(miller-test 2821)
; #f

; ^ is not fooled by Charmichael numbers

