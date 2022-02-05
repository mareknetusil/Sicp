(define (square x) (* x x))

(define (min2 x y)
    (cond ((< x y) x)
          (else y)))

(define (min3 x y z)
    (define min-xy (min2 x y))
    (cond ((< z min-xy) z)
          (else min-xy)))

(define (bigger-squares x y z)
        (- (+ (square x) (square y) (square z))
           (square (min3 x y z))))
