(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))


(define (obdelnikove f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx))


(define (Simpsonovo f a b dx)
    (define (move_by x n)
        (+ x (* dx n)))
    (define (next x) (move_by x 2))
    (define (area x)
        (+ (f x)
           (* 4 (f (move_by x 1)))
           (f (move_by x 2))))
    (define coef (/ dx 3))
    (* (sum area (next a) next b)
       coef))


; Otestuj i n = 10 a n = 10000
