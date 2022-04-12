; ===================================
; 2.5 Systems with Generic Operations
; ===================================

; -----------------------------------
; 2.5.1 Generic Arithmetic Operations
; -----------------------------------

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; real numbers
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


; rational numbers
(define (install-rational-package)
  ;; internal
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


; complex numbers
; NOTE: tahle procedura by mela podle me nainstalovat
;   i vazane baliky pro komplexni cisla z predchozi casti!
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
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

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; exercise 2.77
; real-part, imag-part, magnitude a angle jiz jsou definovane pomoci apply-generic,
; jejich zavolani tedy najde v tabulce (op, '(complex)), coz je odkaze samy na sebe,
; ale tentokrat s argumentem, ktery je bez tagu 'complex
; apply-generic se zavola 2x

; exercise 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (and (number? contents) (eq? type-tag 'scheme-number))
      contents
      (cons type-tag contents)))

; NOTE(2.79+2.80): Co se tady kurva vubec mysli tim, nainstaluj do generic arithmetic package?
; A to mam u 2.79 vypisovat kazdou vypicenou kombinaci (tech je 9)? Kombinace typu jsou
; cilem te dalsi casti, prece!
; exercise 2.79
(define (install-generic-arithmetic-package)
  (define (equ? x y)
    ; TODO!!
  'done)

; exercise 2.80
(define (=zero? x)
  (apply-generic '=zero? x))
(put '=zero? '(scheme-number) (lambda (x) (= x 0)))
(put '=zero? '(rational) (lambda (x) (= (denom x) 0)))
(put '=zero? '(complex) (lambda (x) (and (= (real-part x) 0)
                                         (= (imag-part x) 0))))

; aby fungoval denom, musim podobne jako u komplexnich
; cisel zavest wrapper kolem selectoru


; ---------------------------------------
; 2.5.2 Combining Data of Different Types
; ---------------------------------------
; na hovno reseni:
(define (add-complex-to-schemnum z x)
  (make-from-real-imag (+ (real-part z) x) (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemnum z x))))

; Coersion
; --------
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coersion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coersion type1 type2))
                      (t2->t1 (get-coersion type2 type1)))
                  (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; Hierarchies of types
; --------------------

; Inadequacies of hierarchies
; ---------------------------
; exercise 2.81
; a) apply-generic se zacykli
; b) apply-generic funguje spravne v tom smyslu, ze pokud neni procedura, ktera zere
;   dane 2 stejne typy, tak proste neni a hotovo. Pridani t->t coerce do tabulky,
;   ale zpusobi bug!
; c) 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coersion type1 type2))
                      (t2->t1 (get-coersion type2 type1)))
                  ; Pridana prvni vetev
                  (cond ((eq? type1 type2) (error "No method for these types"
                                                  (list op type-tags)))
                        (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; exercise 2.82
(define (apply-generic op . args)
  (define (coerce-t1->t2 a t2)
    (let ((type-a (type-tag a)))
      (if (eq? type-a t2)
          '(#t a)
          (let ((t1->t2 (get-coersion type-a t2)))
            (if t1->t2
                '(#t (t1->t2 a))
                (#f nil))))))


  (define (try-coerce-iter type-tags args)
    (if (null? type-tags)
        (error "No method for these types"
               (list op (map type-tag args)))
        (let ((type (car type-tags)))
          (let ((converted-args
                  (fold-left (lambda (p l) (cons (and (car p) (car l))
                                           (cons (cdr p) (cdr l))))
                             (#t nil)
                             (map (lambda (a) (coerce-t1->t2 a type)) args))))
            (if (car converted-args)
                ; Zde zase taham zbytecne tagy, ktery jsou stejny, ale je to primocary
                (let ((proc (get op (map type-tag (cdr converted-args)))))
                  (if proc
                      (apply proc (map contents args))
                      (try-coerce-iter (cdr type-tags) args)))
                (try-coerce-iter (cdr type-tags) args))))))


  ; Zde by se hodilo vytahat z type-tags duplicity, ale JEBU!
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-coerce-iter op type-tags args)))))


; exercise 2.83
; TODO
; JAK TADY MAM KURVA RESIT Z VS R??
; Prepokladam, ze mam zaregistrovany typy integral a real,
; ktery wrappujou scheme-number
(define (raise-int z)
  (make-rational (content z) 1))
(define (raise-rat q)
  (make-real (/ (numer q) (denom q))))
(define (raise-real r)
  (make-complex-from-real-imag (content r) 0))

(put 'raise '(integer) raise-int)
(put 'raise '(rational) raise-rat)
(put 'raise '(real) raise-real)

; exercise 2.84
; TODO

; exercise 2.85
; TODO

; exercise 2.86
; TODO


; -------------------------------
; 2.5.3 Example: Symbolic Algebra
; -------------------------------
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error ("Polys not in same var: ADD-POLY" (list p1 p2)))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ; ⟨ procedures same-variable? and variable? from section 2.3.2 ⟩
  ;; representation of terms and term lists
  ; ⟨ procedures adjoin-term . . . coeff from text below ⟩
  (define (add-poly p1 p2) . . . )
  ;⟨ procedures used by add-poly ⟩
  (define (mul-poly p1 p2) . . . )
  ; ⟨ procedures used by mul-poly ⟩
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                   (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1)
                                 (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (const term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; exercise 2.87
(put '=zero? '(polynomial)
     lambda (p) (empty-termlist? (term-list p)))

; exercise 2.88


