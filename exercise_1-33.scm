
(define (filtered-accumulate op init term a next b filter)
    (define (iter a result)
        (define next_res
            (if (filter a)
                (op (term a) result)
                result))
        (if (> a b)
            init 
            (iter (next a) next_res)))
    (iter a init))

(define (sum_prime_squares a b)
    (load "exercise_1-21.csm")
    (define (square x) (* x x))
    (define (next x) (+ x 1))
    (filtered-accumulate + 0 square a next b prime?))
