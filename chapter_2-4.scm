; -----------------------------------------
; 2.4.1 Representations for Complex Numbers
; -----------------------------------------

; mam API:
; make-from-real-imag, real-part, imag-part
; make-from-mag-ang, magnitude, angle

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; implementace - real imag repr
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

; implementace - mag ang repr
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))


; -----------------------------------------
; 2.4.2 Tagged data
; -----------------------------------------
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

; prejmenovane
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; 'genericke'
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))


; ----------------------------------------------
; 2.4.3 Data-Directed Programming and Additivity
; ----------------------------------------------
; v OOP se jedna o polymorfismus
; v C++ k tomu slouzi virtual table, ktera je podobna sloupci ve Figure 2.22

; predpokladam, ze mam
;   (put <op> <type> <item>)
;   (get <op> <type>)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part ('rectangular) real-part) ; NOTE: proc tady byly v textu zavorky kolem rectangular?
  (put 'imag-part ('rectangular) imag-part) ; NOTE: asi vim! To je kvuli apply-generic
  (put 'magnitude ('rectangular) magnitude)
  (put 'angle ('rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part ('polar) real-part)
  (put 'imag-part ('polar) imag-part)
  (put 'magnitude ('polar) magnitude)
  (put 'angle ('polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; exercise 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        (else (error "unknowsn expression type: DERIV" exp))))

; prepsano
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a) deriv implementace se presunuly do tabulky. Cisla a promenne nemaji tag, takze
; pro ne to nejde udelat.
; b)
(define (install-deriv)
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (product-deriv exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)
  'done)
; c)
(define (make-div d1 d2)
  (list '/ d1 d2))
(define (divisor exp)
  (cadr exp))
(define (dividend exp)
  (caddr exp))
(define (install-deriv-div)
  (define (div-deriv exp var)
    (make-div (make-sub
                (make-product (deriv (divisor exp))
                              (dividend exp))
                (make-product (divisor exp)
                              (deriv (dividend exp))))
              (let ((d (dividend exp)))
                (make-product d d))))
  (put 'deriv '/ make-div)
  'done)
; d)
; je to takhle jednoduchy?
(define (install-deriv)
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (product-deriv exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
  (put '+ 'deriv sum-deriv) ; tady jsem to prohodil
  (put '* 'deriv product-deriv) ; tady jsem to prohodil
  'done)

; exercise 2.74
; OKECAT!


; Message passing
; ---------------
(define (make-from-real-imag x y)
  (define (dispatch op)
    ((eq? op 'real-part) x)
    ((eq? op 'imag-part) y)
    ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
    ((eq? op 'angle) (atan y x))
    (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)))
  dispatch)

(define (apply-generic op arg) (arg op))

; exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    ((eq? op 'real-part) (* r (cos a)))
    ((eq? op 'imag-part) (* r (sin a)))
    ((eq? op 'magnitude) r)
    ((eq? op 'angle) a)
    (else (error "Unknown op: MAKE-FROM-MAG-ANG" op)))
  dispatch)

; exercise 2.76
; explicit dispatch - na hovno ve vsech pripadech
; data directed - lze snadno pridavat operace bez zmeny stavajicich dat,
;   musi se ale vzdy pridat do tabulky, tj. vice nez jeden install muze byt potreba
;   pro pouziti novych typu se musi metody pridat do tabulky
; message passing - pro nove operace se musi sahnout do stavajiciho kodu,
;   pro pridani novych typu neni potreba zadna prace navic
; NOTE: metody je samozrejme mozny i kombinovat

