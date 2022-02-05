; 1.3 high-order procedures

; ===================
; 1.3.1 Procedures as Arguments
; ===================

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

; konverguje k pi/8
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; common verze
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; lepsi sum-cubes
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

; lepsi sum-integers
(define (sum-integers a b)
  (define (identity x) x)
  (sum identity a inc b))

; lepsi pi-sum
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; INTEGRALY
; Obdelnikove pravidlo
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; exercise 1.29: Simpsonovo pravidlo
; na trech po sobe jdoucich bodech interpoluje
; funkci f kvadratickym polynomem
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

; fun-fact: pro n = 100 a 1000 je lepsi, ale porovnej
; s obdelnikovym pro n = 10 a 10000 :)

; exercise 1.30: iterativni sum
(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))))
    (iter a 0))

; exercise 1.31: zobecneny soucin 
; rekurzivni
(define (product_rec term a next b)
    (if (> a b)
        1
        (* (term a)
           (product_rec term (next a) next b))))

; iterativni
(define (product_iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

; faktorial
(define (factorial x)
    (define (id x) x)
    (define (next x) (+ x 1))
    (product_iter id 1 next x))

; pi/4
(define (pi_4 n)
    (define (next x) (+ x 2))
    (define (val x)
        (/ (* (- x 1) (+ x 1))
           (* x x)))
    (product_iter val 3 next n))

; exercise 1.32: accumulate (fold)
; rekurzivni
(define (accumulate_rec op init term a next b)
    (if (> a b)
        init
        (op (term a)
            (accumulate_rec op init term (next a) next b))))

; iterativni
(define (accumulate_iter op init term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (op (term a) result))))
    (iter a init))

; sum pomoci akumulatoru
(define (sum term a next b)
    (accumulate_iter + 0 term a next b))
; product pomoci akumulatoru
(define (product term a next b)
    (accumulate_iter * 1 term a next b))

; exercise 1.33: accumulate s filtrem
(define (filtered-accumulate op init term a next b filter)
    (define (iter a result)
        (define next_res
            (if (filter a)
                (op (term a) result)
                result))
        (if (> a b)
            result
            (iter (next a) next_res)))
    (iter a init))

; a)
(define (sum-prime-squares a b)
    (load "exercise_1-21.scm")
    (define (square x) (* x x))
    (define (next x) (+ x 1))
    (filtered-accumulate + 0 square a next b prime?))
; b)
; pouzivam built-in gcd fci
(define (sum-relative-primes n)
  (define (filter x) (= (gcd x n) 1))
  (filtered-accumulate + 0 identity 1 inc n filter))


; ===================
; 1.3.2 Constructing Procedures Using lambda
; ===================

; LAMBDA
; pi-sum pomoci lambd
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; integral s lambdou
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; LET
; ukazka let
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; v ramci let neni mozne delat cross reference
(define x 10)
(let ((x 3)        ; x == 3
      (y (+ x 2))) ; x == 10
  (* x y))

; exercise 1.34
(define (f g) (g 2))
(f f) ; (2 2) je kravina


; ===================
; 1.3.3 Procedures as General Methods
; ===================
; definice z predchozich kapitol a ukolu
(define (average a b) (/ (+ a b) 2))
(define (close-enough? a b) (< (abs (- a b)) 0.01))
; puleni intervalu
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

; kontrakce - d(Ax, Ay) <= a * d(x, y), 0 <= a < 1
; Banachova veta o pevnem bode (dukaz x_n = Ax_{n-1})
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; odmocnina - spatna
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))
; odmocnina - lepsi
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

; exercise 1.35
; x = (x + 1)/x => (x^2 - x - 1)/x = 0

; exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; average dumping
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 2.0)

;exercise 1.37
; iterativni verze
(define (cont-frac n d k)
  (define (cont-frac-iter n d k prev)
    (let ((n-k (n k))
          (d-k (d k)))
      (if (= k 0)
          prev
          (let ((new-prev (/ n-k (+ d-k prev))))
            (cont-frac-iter n d (- k 1) new-prev)))))
  (cont-frac-iter n d k 0))

; rekurzivni verze
(define (cont-frac n d k)
  (define (cont-frac-iter n d l)
    (if (> l k)
        0
        (/ (n l)
           (+ (d l) (cont-frac-iter n d (+ l 1))))))
  (cont-frac-iter n d 1))

; k = 5 je uz blizko na desetiny, k = 10 na tisiciny
(define (phi-k k)
  (/ 1 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))

; exercise 1.38
(define (D i)
  (let ((n (quotient i 3))
        (p (remainder i 3)))
    (if (< p 2)
        1
        (* (+ n 1) 2))))

; D-print je nejake dojebane a chova se posrane :/
(define (D-print i)
  (let ((res (D i)))
    (newline)
    (display res)
    res))

(define (e-k k)
  (+ (cont-frac (lambda (i) 1.0)
                D
                k)
     2))

; exercise 1.39
(define (tan-n x i)
  (if (= i 1)
      x
      (- 0 (* x x))))

(define (tan-d i)
  (- (* 2 i) 1))

(define (tan-c-f x k)
  (cont-frac (lambda (k) (tan-n x k))
             tan-d
             k))

; ===================
; 1.3.4 Procedures as Returned Values
; ===================
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

; Newtonova metoda x_{n+1} = x_n - f/Df
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
; Bacha na arctan a podobny zmrdy!!

(define (sqrt x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; s dampingem
(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (/ x y)) average-damp 1.0))
; newtonovou metodou
(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x)) newton-transform 1.0))

; exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a x x) (* b x) c)))

; exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
; (double (double double)) 16-ti nasobi efekt

; exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; exercise 1.43
; iterativni
(define (repeated f n)
  (define (repeated-iter f k x)
    (if (= k 0)
        x
        (repeated-iter f (- k 1) (f x))))
  ; closure?
  (lambda (x) (repeated-iter f n x)))

; rekurzivni
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

; exercise 1.44
(define (smooth f dx)
    (lambda (x)
      (let ((f1 (f (- x dx)))
            (f2 (f x))
            (f3 (f (+ x dx))))
        (/ (+ f1 f2 f3) 3.0))))
(define (smooth-n f dx n)
  (repeated (smooth f dx) n))

;exercise 1.45
; nefunguje
(define (fourth-root x)
  (fixed-point-of-transform
    (lambda (y) (/ x (* y y y))) average-damp 1.0))
; dvojty dumping
(define (fourth-root x)
  (fixed-point-of-transform
    (lambda (y) (/ x (* y y y))) (repeated average-damp 2) 1.0))

; n-ta odmocnina
(define (n-root n x)
  (let ((f (lambda (y) (/ x (expt y (- n 1)))))
        (damp (repeated average-damp (quotient n 2))))
    (fixed-point-of-transform f damp 1.0)))

; exercise 1.46
; verze, kde good? vyzaduje jeden parametr
(define (iterative-improve good? improve)
  (define (iter-val good? improve x)
    (if (good? x)
        x
        (iter-val good? improve (improve x))))
  (lambda (x) (iter-val good? improve x)))
; verze, kde good? vyzaduje dva parametry
(define (iterative-improve good? improve)
  (define (iter-val good? improve x)
    (let ((next (improve x)))
      (if (good? x next)
          next
          (iter-val good? improve next))))
  (lambda (x) (iter-val good? improve x)))

; sqrt
; vyzaduje guess s jednim parametrem
(define (sqrt x)
  (define (improve guess) (average guess (/ x guess)))
  (define (good? guess) (< (abs (- (* guess guess) x)) 0.001))
  (let ((sqrt-x (iterative-improve good? improve)))
    (sqrt-x x)))

; fixed-point
; vyzaduje guess se dvema parametry
(define (fixed-point f)
  (define tolerance 0.0001)
  (define (good? prev next) (< (abs (- prev next)) tolerance))
  (lambda (x) ((iterative-improve good? f) x)))
