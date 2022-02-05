; 2.2 Hierarchical Data and the Closure Property

; =============================
; 2.2.1 Representing Sequences
; =============================

; racket - nil = '()
;        - neprinti list zjednodusene, ale pomoci cons

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
; note - neresi pripad n > len(items)

; rekurzivni
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; iterativni
(define (length items)
  (define (length-iter l count)
    (if (null? l)
        count
        (length-iter (cdr l) (+ 1 count))))
  (length-iter items 0))

; append - built-in funkce

; List operations
; exercise 2.17
; divny, ze chteji vratit posledni pair a ne posledni hodnotu
(define (last-pair items)
  (let ((tail (cdr items)))
    (if (null? tail)
        items
        (last-pair tail))))

; exercise 2.18
; rekurzivni - hnus
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

; iterativni
(define (reverse items)
  (define (reverse-iter items acc)
    (if (null? items)
        acc
        (reverse-iter (cdr items)
                      (cons (car items) acc))))
  (reverse-iter items nil))

; exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination
                       coin-values))
                 (cc (- amount
                        (first-denomination
                          coin-values))
                     coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
; na poradi minci nezalezi
; pokud nejsou sestupne, tak je algoritmus prumerne drazsi

; exercise 2.20
; rekurzivni
(define (filter p xs)
  (cond ((null? xs) xs)
        ((p (car xs))
         (cons (car xs) (filter p (cdr xs))))
        (else (filter p (cdr xs)))))

(define (same-parity x . xs)
  (let ((par (if (odd? x) odd? even?)))
    (cons x (filter par xs))))

; Mapping over lists
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(scale-list (list 1 2 3 4 5) 10)

; (define (map proc items)
;   (if (null? items)
;       nil
;       (cons (map (car items))
;             (map proc (cdr items)))))

; pomoci map
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

; exercise 2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square x)
            (square-list (cdr items)))))
(define (square-list items)
  (map square items))

; exercise 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                            answer))))
  (iter items nil))
; zrejme to produkuje obracene list (podobne jsem napsal iterativni reverse)
; prohozeni poradi v cons nefunguje, jelikoz se tim nezmeni struktura vnoreni,
; list vlastne neni ani tak tvoren poradim v cons, ale "hloubkou" v hierarchii

; exercise 2.23
; proc tohle reseni nefunguje?
(define (for-each proc items)
  (if (null? items)
      #t
      ((proc (car items))
       (for-each proc (cdr items)))))
; funguje
(define (for-each proc items)
  (map proc items)
  #t)


; =============================
; 2.2.2 Hierarchical Structures
; =============================
(define x (cons (list 1 2) (list 3 4)))
(length x)
; zakerny problem!
; proc je (length x) = 3, ale (length (list x x)) = 2?
; duvod: cons != list! potreba si uvedomit, co dela list na pozadi

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves (list x x))

; exercise 2.24
; (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 '())) '())) '()))

; exercise 2.25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

; exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; slouceny list
(cons x y) ; pair o dvou listech
(list x y) ; list o dvou listech, tj jeste jeden cons a nil navic

; exercise 2.27
; iterativni v urovni listu, rekurzivni mezi ruznymi urovnemi
(define (deep-reverse items)
  (define (reverse-iter items acc)
    (if (null? items)
        acc
        (let ((item (if (pair? (car items))
                        (reverse-iter (car items) nil)
                        (car items))))
          (reverse-iter (cdr items)
                        (cons item acc)))))
  (reverse-iter items nil))

; moje helper fce
(define (traverse-tree x op f init)
  (cond ((null? x) init)
        ((not (pair? x)) (f x))
        (else (op (traverse-tree (car x) op f init)
                  (traverse-tree (cdr x) op f init)))))

; exercise 2.28
; kratke, ale asi pomale rekurzivni reseni
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))
; kratsi pomoci traverse
(define (fringe x)
  (traverse-tree x append list nil))
(fringe (list x x))

; exercise 2.29
; proc nam tady vnucujou list misto pair?
; ten, kdo tohle vymyslel, je retard
(define (make-mobile left right)
  (list left right))
(define (make-branch l structure)
  (list l structure))

; a)
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
; b)
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile)
         (+ (total-weight (car (cdr (left-branch mobile))))
            (total-weight (car (cdr (right-branch mobile))))))
        (else mobile)))
; c)
(define (balanced x)
  (define (balanced-node node)
    (define (moment branch)
      (* (car branch) (total-weight (car (cdr branch)))))
    (= (moment (left-branch node))
       (moment (right-branch node))))
  (if (pair? x)
      (and (balanced-node x)
           (balanced (car (cdr (left-branch x))))
           (balanced (car (cdr (right-branch x)))))
      #t))
(define x
  (make-mobile
    (make-branch 5 (make-mobile
                     (make-branch 1 4)
                     (make-branch 2 2)))
    (make-branch 1 (make-mobile
                     (make-branch 2 10)
                     (make-branch 1 20)))))
x
(balanced x)

; d)
; program se vyrazne zjednodusi, protoze list je retardovany
(define (make-mobile left right)
  (cons left right))
(define (make-branch l structure)
  (cons l structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile)
         (+ (total-weight (cdr (left-branch mobile)))
            (total-weight (cdr (right-branch mobile)))))
        (else mobile)))

(define (balanced x)
  (define (balanced-node node)
    (define (moment branch)
      (* (car branch) (total-weight (cdr branch))))
    (= (moment (left-branch node))
       (moment (right-branch node))))
  (if (pair? x)
      (and (balanced-node x)
           (balanced (cdr (left-branch x)))
           (balanced (cdr (right-branch x))))
      #t))

; Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; Tahle sracka nefunguje, radsi se ani neptam proc...
; Asi mam rozbity map
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; exercise 2.30
(define (square x) (* x x))
; direct
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
; map
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

; exercise 2.31
(define (tree-map tree proc)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree proc)
             (proc sub-tree)))
       tree))
(define (square-tree tree) (tree-map tree square))

; exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x))
                          rest)))))


; ==========================================
; 2.2.3 Sequences as Conventional Interfaces
; ==========================================
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

; Sequence Operations
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
; priklad
(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
; priklad
(accumulate + 0 (list 1 2 3 4 5))

; tohle bych nepojmenoval jako enumerate, v pythonu je to ekvivalent range
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
; priklad
(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
; priklad
(enumerate-tree (list 1 (list 2 (list 3 4) 5)))

(define (sum-odd-squares tree)
  (accumulate
    + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
    cons
    nil
    (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
    cons
    nil
    (map square (map fib (enumerate-interval 0 n)))))
(list-fib-squares 10)

; atd.

; exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))
; note: zde mi doslo, ze se mi plete rekurzivni charakter accumulate
; a v hlave mam list casto obracene (to se projevilo uz u append)
; tj (list 3 2 1) = x^2 + 2x + 3

; exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))
(count-leaves (list 1 (list 2 (list 3 4) 5)))

; exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector M v)
    (map (lambda (u) (dot-product u v)) M))
(define (transpose M)
  (accumulate-n cons nil M))
(define (matrix-*-matrix M N)
  (let ((cols (transpose N)))
    (map (lambda (row) (matrix-*-vector cols row)) M)))

; testy
(define u (list 1 1 1))
(define v (list 0 2 1))
(define I (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define A (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(dot-product u v)
(matrix-*-vector I u)
(matrix-*-vector A u)
(for-each display A)
(for-each display (transpose A))
(for-each display (matrix-*-matrix I A))
(for-each display (matrix-*-matrix A I))

; exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
; 3/2
; 1/6
; (list 1 (list 2 (list 3 nil)))
; (list (list (list nil 1) 2) 3)
; fold-left a fold-right jsou stejne pro asociativni operace
; NOTTRUE!!! Toto je pravda pouze tehdy, pokud initial hodnota
; je neutralni vzhledem k op, coz nemusi byt pravda!

; exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))


; Nested Mappings
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; on celou dobu existuje cadr?? WTF???
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (j) (list i j))
                                   (enumerate-interval 1 (-i 1))))
                            (enumerate-interval 1 n)))))

; NOTE: az tady predstavujou komentare?
(define (permutations s)
  (if (null? s)     ; empty set?
      (list nil)    ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (perumations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; zjednodusene prime-sum-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (i) (map (lambda (pair) (cons i pair))
                            (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples-s n s)
  (filter (lambda (t) (= s (accumulate + 0 t)))
          (unique-triples n)))

; exercise 2.42
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row
                                      k
                                      rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queens-cols (- k 1))))))
  (queens-cols board-size))

(define nil '())
(define pos cons)
(define empty-board nil)
(define (adjoin-position row col rest-of-queens)
  (cons (pos row col) rest-of-queens))
(define (safe? positions)
  (define (collision? pos1 pos2)
    (cond ((= (car pos1) (car pos2)) #f)
          ((= (cdr pos1) (cdr pos2)) #f)
          ((= (abs (- (car pos1) (car pos2)))
              (abs (- (cdr pos1) (cdr pos2))))
           #f)
          (else #t)))
  (let ((new-queen (car positions))
        (rest (cdr positions)))
    (accumulate (lambda (x y) (and x y))
                #t
                (map (lambda (p) (collision? new-queen p))
                     rest))))

; exercise 2.43
; super slow queens
(define (slow-queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row
                                      k
                                      rest-of-queens))
                   (queens-cols (- k 1))))
              (enumerate-interval 1 board-size)))))
  (queens-cols board-size))
; vysvetleni: prvni verze ma vygenerovane kralovny pro (k-1) a vytvori k ni vsechny
; moznosti jak pridat kralovnu do dalsiho sloupce
; druha verze ma vygenerovane kralovny pro k-ty sloupec a ke kazde z nich vytvori
; vsechny moznosti jak poskladat kralovny do (k-1) sloupcu, tj. hromada opakovane prace!



; 2.2.4 Example: A Picture Language
; exercise 2.44
(define (up-slip painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; exercise 2.45
; NOTE: jak vratit rekurzivni lambdu? Nejde to lip nez takhle?
(define (split op1 op2)
  (define (tmp painter n)
    (if (= n 0)
        painter
        (let ((smaller (tmp painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  (lambda (painter n) (tmp painter n)))

;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect (origin-frame frame)
;              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
;                        (scale-vect (ycor-vect v) (edge2-frame frame))))))
; exercise 2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect u v)
  (define (sum-coors u v sel)
    (+ (sel u) (sel v)))
  (make-vect (sum-coors u v xcor-vect)
             (sum-coors u v ycor-vect)))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (sub-vect u v)
  (add-vect u (scale-vect -1 v)))

; exercise 2.47
; VERZE 1
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define (edge2-frame frame)
  (cadr (cdr frame)))

; VERZE 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (edge2-frame frame)
  (cdr (cdr frame)))
; zbytek stejny


; exercise 2.48
(define (make-segment u v)
  (cons u (add-vect u v)))
(define start-segment car)
(define end-segment cdr)

; exercise 2.49
; a
(define outline-painter
  (let ((bl (make-vect 0 0))
        (br (make-vect 1 0))
        (tr (make-vect 1 1))
        (tl (make-vect 0 1)))
    (segments->painter
      (list (make-segment bl br)
            (make-segment br tr)
            (make-segment tr tl)
            (make-segment tl bl)))))
; b
(define x-painter
  (let ((bl (make-vect 0 0))
        (br (make-vect 1 0))
        (tr (make-vect 1 1))
        (tl (make-vect 0 1)))
    (segments->painter
      (list (make-segment bl tr)
            (make-segment tl br)))))

; c
(define diamond-painter
  (let ((bl (make-vect 0.0 0.5))
        (br (make-vect 0.5 0.0))
        (tr (make-vect 1.0 0.5))
        (tl (make-vect 0.5 1.0)))
    (segments->painter
      (list (make-segment bl br)
            (make-segment br tr)
            (make-segment tr tl)
            (make-segment tl bl)))))

; d
; WUT???

; Transforming and combining painters
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         split-point
                                         (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; exercie 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
; rotate180 + rotate 270 - na tyhle seru

; exercise 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up (transform-painter painter1
                                       (make-vect 0.0 0.0)
                                       (make-vect 1.0 0.0)
                                       split-point))
          (paint-down (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.5)
                                         (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))
; druha varianta
(define (below painter1 painter2)
  (rotate90
    (beside (rotate270 painter1)
            (rotate270 painter2))))
