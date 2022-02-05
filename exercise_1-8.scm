(define (square x) (* x x))
(define (cube x) (* (square x) x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (/ (- x (cube guess))
             x))
     0.0000001))

(define (third-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (third-root-iter (improve guess x) x)))

(define (third-root x)
  (third-root-iter 1.0 x))
