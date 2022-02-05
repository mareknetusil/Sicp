(define (even? n)
    (= (remainder n 2) 0))

(define (double a) (* 2 a))
(define (half a) (/ a 2))

(define (mul a b)
    (mul_iter 0 a b))

(define (mul_iter res a b)
    (cond ((= b 0) res)
          ((even? b) (mul_iter res (double a) (half b)))
          (else (mul_iter (+ res a) a (- b 1)))))
