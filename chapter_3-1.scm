; 3 Modularity, Objects, and State

; ==============================
; 3.1 Assignment and Local State
; ==============================

; ---------------------------
; 3.1.1 Local State Variables
; ---------------------------

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUT"
                       m))))
  dispatch)


; exercise 3.1
(define (make-accumulator init)
  (lambda (value)
    (set! init (+ init value))
    init))

; execise 3.2
(define (make-monitored f)
  (let ((counter 0))
    (define (call arg)
      (set! counter (+ counter 1))
      (f arg))
    (define (dispatch m)
      (if (eq? m 'how-many-calls?)
          counter
          (call m)))
    dispatch))

; exercise 3.3
(define (make-account balance pass)
  (define (wrong-password amount)
    "incorrect password")
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m given-pass)
    (if (eq? given-pass pass)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUT"
                           m)))
        wrong-password))
  dispatch)

; exercise 3.4
(define (make-account balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((counter 0))
    (define (wrong-password amount)
      (if (< counter 2)
          (begin (set! counter (+ counter 1))
                 "Incorrect password")
          (call-the-cops)))

    (define (dispatch m given-pass)
      (if (eq? given-pass pass)
          (begin
            (set! counter 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUT"
                               m))))
          wrong-password))
    dispatch))


; --------------------------------------------
; 3.1.2 The Benefits of Introducing Assignment
; --------------------------------------------
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

; exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 n)
  (define (integral-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (- x2 x1) (- y2 y1) (monte-carlo n integral-test)))

(define (estimate-pi trials)
  (define R 100000) ; VLIV NA PRESNOST!
  (define (square x) (* x x))
  (define (in-circle? x y)
    (< (+ (square x) (square y)) (square R)))
  (/ (estimate-integral in-circle? 0 R 0 R trials) (* 0.25 (square R))))

; exercise 3.6
(define rand
  (let ((x random-init))
    (define (reset new-val)
      (set! x new-val)
      x)
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (reset (rand-update x)))
            ((eq? m 'reset)
             reset)
            (else (error "Unknown command!"))))
    dispatch))


; ----------------------------------------
; 3.1.3 The Cost of Introducing Assignment
; ----------------------------------------
; simplified withdraw
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

; p.316 - POZOR na vyznam referenci!
; (define paul-acc peter-acc)
; Chovani je podobne jako v Pythonu
; tj. paul-acc NENI peter-acc!
; Pokud zmenim peter-acc, nema to ZADNY vliv na paul-acc.
; Pokud ale zmenim objekt, ktery REFERUJE peter-acc,
; paul-acc tim zmenim!


; Pitfalls of imperative programming
; ----------------------------------
(define (fun-factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (imp-factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product)) ; DULEZITE PORADI!
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


; exercise 3.7
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUT"
                       m))))

  dispatch)

(define (guard-acc account pass)
  (define (wrong-password amount)
    "incorrect password")

  (define (dispatch-safe given-pass)
    (if (eq? given-pass pass)
        account
        wrong-password))

  dispatch-safe)

(define (make-safe-account balance pass)
  (guard-acc
    (make-account balance)
    pass))

(define (make-joint account src-pass dst-pass)
  (guard-acc
    (account src-pass)
    dst-pass))


; exercise 3.8
; inspiraci uskali post-inc v c/c++
(define f
  (let ((state 0))
    (lambda (x)
      (let ((tmp state))
        (set! state x)
        tmp))))

