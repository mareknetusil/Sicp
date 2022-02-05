; 2.1 Introduction to Data Abstraction

; selectors x constructors

;=================================
; 2.1.1 Example: Arithmetic Operations for Rational Numbers
;=================================

(define make-rat cons)
(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; automaticke vykraceni na zakladni tvar pri vzniku
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


; exercise 2.1
(define (sgn x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))
; zde mi vadi, ze se v jednom let nemuzu odkazat na n-abs a d-abs
; je teda situace, kdy je let opravdu vyrazne lepsi nez define?
(define (make-rat n d)
  (let ((n-abs (abs n))
        (d-abs (abs d))
        (sign (* (sgn n) (sgn d))))
    (let ((g (gcd n-abs d-abs)))
      (cons (* sign (/ n-abs g)) (/ d-abs g)))))
; verze s define - NEFUNGUJE, PROC?, na strane 88 je podobny priklad
; POSLAT!!!!!!
(define (make-rat n d)
  (define n-abs (abs n))
  (define d-abs (abs d))
  (define sign (* (sgn n) (sgn d)))
  (define g (gcd n-abs d-abs))
  (cons (* sign (/ n-abs g)) (/ d-abs g)))

; strana 120 - chapu, o co se snazi v prikladu, ze kraceni zlomku lze provest
; az v selectorech a nemusi se delat vzdy v constructoru, ale v tomhle pripade
; je to neuzitecna blbost :)

; exercise 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (if (> x y)
      (error "Meze musi byt neklesajici!")
      (cons x y)))

(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment I)
    (let ((A (start-segment I))
          (B (end-segment I)))
      (make-point (/ (+ (x-point A) (x-point B)) 2)
                  (/ (+ (y-point A) (y-point B)) 2))))

; exercise 2.3
; NOTE: nejmensi pocet cisel pro reprezentaci obdelniku je 5!
; Vyjimka: strany jsou vzdy rovnobezne s osami -> potom staci 4 hodnoty

; definice obsahu a obvodu zavisi pouze na stranach
(define (rect-area A)
  (let ((ab (sides A)))
    (* (car ab) (cdr ab))))

(define (rect-perim A)
  (let ((ab (sides A)))
    (* (+ (car ab) (cdr ab))
       2)))

; obdelnik rovnobezny s osami
(define (make-rect A C)
  (cons A C))
(define (sides rect)
  (let ((A (car rect))
        (C (cdr rect)))
    (cons (abs (- (x-point C) (x-point A)))
          (abs (- (y-point C) (y-point A))))))

; obecny obdelnik
(define make-rect-simple cons) ; (make-rect-simple a b)
(define make-full-pos cons) ; (make-full-pos S angle)
(define (make-rect a b S alpha)
  (cons (make-rect-simple a b)
        (make-full-pos S alpha)))
(define (sides rect)
  (car rect))

; ==================================
; 2.1.3 What is Meant by Data?
; ==================================
; definice datovych typu (zakladnich i kompozitnich) pomoci funkci
; to je mozny, jelikoz funkce maji dostatecne bohatou strukturu
; Zakladem pro vnimani funkci jako prirozenych cisel jsou Peanovy axiomy

; priklad zavedeni cons
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS:" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

; obecne lze libovolne pole vnimat jako fci
; zobrazujici prirozena cisla do libovolne mnoziny

; nevim, co je message passing, pockam si na kapitolu 3

; exercise 2.4
; mam tohle dobre?
; (car (cons x y)) = ((cons x y) (lambda (p q) p)) =
; ((lambda (p q) p) x y) = x
(define (cdr z)
  (z (lambda (p q) q)))

; exercise 2.5
(define (prime-exp z p)
  (define (prime-exp-iter z n)
    (if (= (remainder z p) 0)
      (prime-exp-iter (quotient z p) (+ n 1))
      n))
  (prime-exp-iter z 0))

(define (make-pair a b)
  (* (expt 2 a) (expt 3 b)))
(define (left x) (prime-exp x 2))
(define (right x) (prime-exp x 3))

; exercise 2.6
(define (one f) (lambda (x) (f x)))
(define (two f) (lambda (x) (f (f x))))
(define (plus h g)
  (lambda (f) (lambda (x) ((h f) ((g f) x)))))


; ==================================
; 2.1.4 Extended Exercise: Interval Arithmetic
; ==================================
(define make-interval cons) ; kaslu na check a <= b
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

; exercise 2.7
(define lower-bound car)
(define upper-bound cdr)

; exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; exercise 2.9
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))
; soucet:
; w(x + y) = ((x.b + y.b) - (x.a + y.a))/2 =
; (x.b - x.a)/2 + (y.b - y.a)/2 = w(x) + w(y) q.e.d.
; soucin - predpokladam, ze vsechny hodnoty jsou kladne:
; w(x * y) = (x.b*y.b - x.a*y.a)/2 = (x.b*y.b - x.a*y.b + x.a*y.b - x.a*y.b)/2 =
; ((x.b - x.a)*y.b + x.a*(y.b - y.a))/2 = y.a*w(x) + x.a*w(y) q.e.d.

; exercise 2.10
(define (in-interval v x)
  (not (or (< v (lower-bound x)) (> v (upper-bound x)))))
(define (checked-div-interval x y)
  (if (in-interval 0 y)
      (error "Nelze delit intervalem obsahujicim 0!")
      (div-interval x y)))

; exercise 2.11
; TODO


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; exercise 2.12
(define (make-center-percent c p)
  (let ((w (/ (* c p) 100.0)))
    (make-center-width c w)))
(define (percent i)
  (let ((c (center i))
        (w (width i)))
    (* 100.0 (/ w c))))

; exercise 2.13
; po hromade zanedbani clenu druheho a vyssiho radu dostaneme
; p3 = p1 + p2

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; exercise 2.14
(define R1 (make-center-percent 100 10))
(define R2 (make-center-percent 10000 10))
(par1 R1 R2)
(par2 R1 R2) ; tento interval je uzsi

; exercise 2.15
; sirka intervalu nemeni pro 1/I, jelikoz interval 1 = (1, 1) ma sirku 0,
; par2 se sklada pouze s techto vyrazu, jediny moment, kdy se meni sirky je
; u souctu
; oproti tomu par1 obsahuje hromadu zbytecnych operaci navic, ktere vnasi
; do vysledku neexistujici neurcitost

; exercise 2.16
; NA DEBATU :)
