;;; basic arithmetic
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (abs x) (if (< x 0) (- x) x))
(define (average a b) (/ (+ a b) 2))
(define (double a) (* a 2))
(define (halve a) (/ a 2))
(define (cube x) (* x x x))
(define (plus1 a) (+ a 1))
(define (identity x) x)

;;; >>>>>>>>>>>>>>>>>>> exercise 1.11 >>>>>>>>>>>>>>>>>>>>>>>
;;; 
;;; f(n) = {n                      | n < 3,
;;;         f(n-1)+2f(n-2)+3f(n-3) | n >= 3}
(define (f11 n)
  (cond
   ((< n 3) n)
   (else (+ (f11 (- n 1)) (* 2 (f11 (- n 2))) (* 3 (f11 (- n 3)))))))

;;; f-iter implemented f as tail-recursive
(define (f11-iter n)
  (define (f-iter-sub a b c i)
    (cond ((= i 0) a)
	  ((= i 1) b)
	  ((= i 2) c)
	  (else (f-iter-sub b c (+ (* 3 a) (* 2 b) c) (- i 1)))))
  (f-iter-sub 0 1 2 n))
;;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;; >>>>>>>>>>>>>>>>>>> exercise 1.12 >>>>>>>>>>>>>>>>>>>>>>>
(define (pascal row col)
  (cond ((= col 1) 1)
	((= row col) 1)
	(else (+ (pascal (- row 1) (- col 1))
		 (pascal (- row 1) col)))))
;;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;;; compute square root in newton method
(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) 0.0001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;; compute cube root in newton method
(define (curt x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) 0.0001))
  (define (curt-iter guess)
    (if (good-enough? guess)
	guess
	(curt-iter (improve guess))))
  (curt-iter 1.0))

;;; exponential
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt2 b n)
  (define (expt-iter b n c)
    (if (= n 0)
	c
	(expt-iter b (- n 1) (* c b))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

;;; exercise 1.16
(define (fast-expt2 b n)
  (define (fast-expt-iter b n c)
    (cond ((= n 0) c)
	  ((even? n) (fast-expt-iter (square b) (/ n 2) c))
	  (else (fast-expt-iter b (- n 1) (* c b)))))
  (fast-expt-iter b n 1))

;;; exercise 1.17
(define (prod a b)
  (if (= b 0)
      0
      (+ a (prod a (- b 1)))))
(define (prod-fast a b)
  (cond ((= b 0) 0)
	((even? b) (double (prod-fast a (halve b))))
	(else (+ a (prod-fast a (- b 1))))))

;;; exercise 1.18
(define (prod-fast2 a b)
  (define (prod-fast-iter a b c)
    (cond ((= b 0) c)
	  ((even? b) (prod-fast-iter (double a) (halve b) c))
	  (else (prod-fast-iter a (- b 1) (+ c a)))))
  (prod-fast-iter a b 0))

;;; exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (double (* p q)) (square q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;;; GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;;; PRIME TEST
(define (prime? n)
  (= n (smallest-divisor n)))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m))
				m))
	(else (remainder (* base (expmod base (- exp 1) m))
			 m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

;;; exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes low high)
  (if (< low high)
      (cond ((even? low) (search-for-primes (+ low 1) high))
	    (else (timed-prime-test low) (search-for-primes (+ low 2) high)))))

;;; procedure as an argument
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
;;; recursive version of sum
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a) (sum term (next a) next b))))
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))
  
(define (sum-cubes2 a b)
  (sum cube a plus1 b))
(define (sum-integers2 a b)
  (sum identity a plus1 b))
(define (pi-sum2 a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (next x) (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2.0)) next b)))
;;; => (integral cube 0 1 0.01)
;;; .24998750000000042
;;; => (integral cube 0 1 0.001)
;;; .249999875000001

(define (simpson-integral f a b n)
  (define (term x k)
    (cond ((= k 0) (f x))
	  ((= k n) (f x))
	  ((even? k) (* 2 (f x)))
	  (else (* 4 (f x)))))
  (define h (/ (- b a) n))
  (define (sum term a k)
    (if (> k n)
	0
	(+ (term (+ a (* k h)) k)
	   (sum term a (+ k 1)))))
  (* (/ h 3)
     (sum term a 0)))
;;; => (simpson-integral cube 0 1 100)
;;; 1/4
;;; => (simpson-integral cube 0 1 1000)
;;; 1/4

;;; exercise 1.31.a
;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;	 (product term (next a) next b))))
(define (term1.31 n)
  (define (a n)
    (if (even? n)
	(+ n 2.0)
        (+ n 1.0)))
  (define (b n)
    (if (even? n)
        (+ n 1.0)
        (+ n 2.0)))
  (/ (a n) (b n)))
; (* (product term1.31 1 plus1 1000) 4)

;;; exercise 1.31.b
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

;;; exercise 1.32
;(define (accumulate combiner null-value term a next b)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;; exercise 1.35
;;;(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1)

;;; exercise 1.36
;;;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;;;(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)

(define (deriv f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (f (+ x dx)) (f x))
	 dx))))

(define (newtons-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

(define (newtons-method f guess)
  (fixed-point (newtons-transform f) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

;;; exercise 1.40
;;;(define (cubic a b c)
;;;  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
;;;(newtons-method (cubic 1 2 3) 1)

;;;exercise 1.41
;;;(define (double f)
;;;  (lambda (x) (f (f x))))

;;;exercise 1.42
(define (compose g f)
  (lambda (x) (g (f x))))
;;;exercise 1.43
(define (repeated f count)
  (if (= 1 count)
      f
      (compose f (repeated f (- count 1)))))
;;;exercise 1.44
(define (smooth f)
  (let ((dx 0.1))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))
(define (smooth-repeated f count)
  (repeated smooth count) f)
