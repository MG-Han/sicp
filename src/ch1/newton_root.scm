(define (square x)
  (* x x))
(define (abs x)
  (if (< x 0) (- x) x))
(define (average a b)
  (/ (+ a b) 2))

;compute square root in newton method					
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

;compute cube root in newton method
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
