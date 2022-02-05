(define (product_rec term a next b)
    (if (> a b)
        1
        (* (term a)
           (product_rec term (next a) next b))))

(define (product_iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

(define (factorial x)
    (define (id x) x)
    (define (next x) (+ x 1))
    (product_iter id 1 next x))

(define (pi_4 n)
    (define (next x) (+ x 2))
    (define (val x)
        (/ (* (- x 1) (+ x 1))
           (* x x)))
    (product_iter val 3 next n))
(define (pi n)
    (* 4.0 (pi_4 n)))
