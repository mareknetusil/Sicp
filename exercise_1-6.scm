(load "sqr_root_1-7.csm")

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause) (else else-clause)))


(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x) x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))
