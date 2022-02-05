(define (accumulate_rec op init term a next b)
    (if (> a b)
        init
        (op (term a)
            (accumulate_rec term (next a) next b))))

(define (accumulate_iter op init term a next b)
    (define (iter a result)
        (if (> a b)
            init 
            (iter (next a) (op (term a) result))))
    (iter a init))

(define (sum term a next b)
    (accumulate_iter + 0 term a next b))

(define (product term a next b)
    (accumulate_iter * 1 term a next b))
