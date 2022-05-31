; ==============================
; 3.3 Modeling with Mutable Data
; ==============================

; ----------------------------
; 3.3.1 Mutable List Structure
; ----------------------------

; pozn 17. - toto podle me neni novinka, "garbage" muze vzniknout
; i bez mutable dat

;(define (cons x y)
;  (let ((new (get-new-pair)))
;    (set-car! new x)
;    (set-cdr! new y)
;    new))

; exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x) ; resp: '(b)
(define w (append! x y))
w
(cdr x) ; resp: '(b c d)

; exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

; exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
; (define v (list 'a 'b 'c 'd))
; (define w (mystery v))
; w je prevraceny list '(d c b a)
; v je posledni pair (cons 'a nil)


; Sharing and identity
; --------------------
(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)
z1
(set-to-wow! z1)
z2
(set-to-wow! z2)

; exercise 3.15
; trivialni

; exercise 3.16
(display "exercise 3.16\n")
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
(define a (list 1 2 3))
(define b1 (cons 1 2))
(define b2 (cons b1 b1))
(define b (cons b2 3))
(define c1 (cons 1 2))
(define c2 (cons c1 c1))
(define c (cons c2 c2))
(define d1 (cons 1 2))
(define d2 (cons 3 4))
(define d (cons d1 d2))
(set-car! d1 d)
(count-pairs a)
(count-pairs b)
(count-pairs c)
;(count-pairs d)

;exercise 3.17
(display "exercise 3.17\n")
(define (mem? e l)
  (cond ((null? l) #f)
        ((eq? e (car l)) #t)
        (else (mem? e (cdr l)))))
(define add cons)

(define (count-pairs x)
  (define searched nil)
  (define (count-pairs-iter x)
    (if (or (not (pair? x))
            (mem? x searched))
        0
        (begin
          (set! searched (add x searched))
          (+ (count-pairs-iter (car x))
             (count-pairs-iter (cdr x))
             1))))
  (count-pairs-iter x))

(count-pairs a)
(count-pairs b)
(count-pairs c)
(count-pairs d)

; exercise 3.18
(define (is-loop? l)
  (define searched nil)
  (define (is-loop-iter l)
    (cond ((null? l) #f)
          ((mem? l searched) #t)
          (else (begin
                  (set! searched (add l searched))
                  (is-loop-iter (cdr l))))))
  (is-loop-iter l))
(display "exercise 3.18\n")
(define a (list 1 2 3 4))
a
(is-loop? a)
(define b (make-cycle a))
b
(is-loop? b)

; exercise 3.19
; TODO