(define (P n k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          (else (+ (P (- n 1) (- k 1))
                   (P (- n 1) k)))))
