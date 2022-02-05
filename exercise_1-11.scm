(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3))))))


(define (f_iter n)
    (if (< n 3)
        n
        (f_inner (f 2) (f 1) (f 0) n)))
            

(define (f_inner a b c n)
    (define tmp
            (+ a (* 2 b) (* 3 c)))
    (if (< n 3)
        a
        (f_inner tmp a b (- n 1))))
