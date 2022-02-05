(define (even? n)
    (= (remainder n 2) 0))

(define (exp b n)
    (exp_iter 1 b n))

(define (exp_iter a b n)
    (cond ((= n 0) a)
          ((even? n) (exp_iter a (* b b) (/ n 2)))
          (else (exp_iter (* a b) b (- n 1)))))

