(define (even? n)
    (= (remainder n 2) 0))

(define (double a) (* 2 a))
(define (half a) (/ a 2))

(define (mul-fast a b)
    (cond ((= b 0) 0)
          ((even? b) (double (mul-fast a (/ b 2))))
          (else (+ a (mul-fast a (- b 1))))))
