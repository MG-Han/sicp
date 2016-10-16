;compute square root in newton method
(define (square x) (* x x))
(define (abs x)
  (if (< x 0) (- x) x))
(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.0001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))
  
;compute cube root in newton method
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cube-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-iter (improve-cube guess x) x)))
(define (good-enough-cube? guess x)
  (< (abs (- (improve-cube guess x) guess)) 0.0001))
(define (curt x)
  (cube-iter 1.0 x))

