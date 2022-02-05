; 2.3 Symbolic Data

; ================
; 2.3.1 Quotation
; ================
(define a 1)
(define b 2)
(list a b)

(list 'a 'b)
(list 'a b)

(car '(a b c))
(cdr '(a b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


; exercise 2.53
(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; false
(memq 'red '((red shoes) (blue socks))) ; false
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)


; exercise 2.54
(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? a)))
         (eq? a b))
        (else false)))

; exercise 2.55
; nemam poneti, proste to dela
; ''neco chape jako (' neco)


; 2.3.2 Example : Symbolic Differentitation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "neznamy typ vyrazu: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s)) ; WUT?? caddr?? max existuje cadddr!

(define (make-product m1 m2) (list  '* m1 m2))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num) (and (number? exp) (= exp num)))
; lepsi make-sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; exercise 2.56
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((base 0) 0)
        (else (list '** base exponent))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)

; exercise 2.57
; NOTE: apply!!!!
(define (make-variadic-op op . as) (cons op as))
(define variadic-op-first cadr)
(define (variadic-op-rest op s)
  (let ((rest (cdr (cdr s))))
    (if (null? (cdr rest))
        (car rest)
        (cons op rest)))) ; neco jako *arg v pythonu by se hodilo

(define (make-sum . as) 
  (apply make-variadic-op (cons '+ as)))
(define addend variadic-op-first)
(define (augend s)
  (variadic-op-rest '+ s))

(define (make-product . as)
  (apply make-variadic-op (cons '* as)))
(define multiplier variadic-op-first)
(define (multiplicand p)
  (variadic-op-rest '* p))

; exercise 2.58
; TODO


; ================================
; 2.3.3 Example: Representing Sets
; ================================
; Set as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

; exercise 2.60
; element-of-set? neni duvod menit
(define adjoin-set cons) ; konstatni x linearni
(define union-set append) ; linearni x kvardraticky
; intersetion-set - zde si nejsem jisty, ale myslim, ze lip to nejde

; Set as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))

; exercise 2.61
; NOTE: tohle bude uz z principu prumerne n a ne n/2, to zadani
; je picovina
(define (prelij a b)
  (if (null? a)
      b
      (prelij (cdr a) (cons (car a) b))))
(define (adjoin-set x set)
  (define (adjoin-set-iter x set acc)
    (cond ((null? set) (prelij acc (list x)))
          ((= x (car set))
           (prelij acc set))
          ((< x (car set))
           (prelij acc (cons x set)))
          (else (adjoin-set-iter x
                                 (cdr set)
                                 (cons (car set) acc)))))
  (adjoin-set-iter x set '()))

; exercise 2.62
(define (union-set set1 set2)
  (define (adjoin-set-iter set1 set2 acc)
    (cond ((null? set1) (prelij acc set2))
          ((null? set2) (prelij acc set1))
          ((= (car set1) (car set2))
           (adjoin-set-iter (cdr set1)
                            (cdr set2)
                            (cons (car set1) acc)))
          ((< (car set1) (car set2))
           (adjoin-set-iter (cdr set1)
                            set2
                            (cons (car set1) acc)))
          (else (adjoin-set-iter set1
                                 (cdr set2)
                                 (cons (car set2) acc)))))
  (adjoin-set-iter set1 set2 '()))

; Set as binary trees
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

; exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                      (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                              (right-branch tree)
                              result-list)))))
  (copy-to-list tree '()))

; a) oba plodi stejny list
; b) tree->list-2, myslim rychlejsi, protoze tree->list-1 prasi ten append

; exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; a) okecam, jak to funguje
; b) myslim, ze linearni

; exercise 2.65
; prejmenoval jsem si to, protoze pouzivam union-set ordered listu
(define (union-tree set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (union-set list1 list2))))

(define (intersection-tree set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (intersection-set list1 list2))))


; Sets and information retrieval
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; exercise 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((root (entry set-of-records)))
        (cond ((= given-key (key root)) root)
              ((< given-key (key root))
               (lookup given-key (left-branch set-of-records)))
              (else
                (lookup given-key (right-branch set-of-records)))))))


; =====================================
; 2.3.4 Example: Huffman Encoding Trees
; =====================================

; Representing Huffman trees
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


; The decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

; Sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; ADABBCA

; exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; potreba tento element-of-set? pro nesetridene sety
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "SYMBOL NENI VE STROMU!" symbol))))

; exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (if (null? leafs)
      '()
      (let ((prvni (car leafs))
            (zbytek (cdr leafs)))
        (if (null? zbytek)
            prvni
            (successive-merge
              (adjoin-set (make-code-tree (car zbytek) prvni) (cdr zbytek)))))))

; exercise 2.70
(define rock-tree
  (generate-huffman-tree
    (list (list 'A 2) (list 'GET 2) (list 'SHA 3) (list 'WAH 1)
          (list 'BOOM 1) (list 'JOB 2) (list 'NA 16) (list 'YIP 9))))

(define lyrics
  '( GET A JOB SHA NA NA NA NA NA NA NA NA
     GET A JOB SHA NA NA NA NA NA NA NA NA
     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
     SHA BOOM))
(encode lyrics rock-tree)

; delka kodovani je 84 bitu
; pri fixnim kodovani: 3*36 - 108 bitu


; exercise 2.71
; the most frequent ma 1 bit
; the least frequent ma n - 1 bitu

; exercise 2.72
; pokud pocet moznych symbolu = m a delka kodovaneho textu = n,
; potom je slozitost zakodovani symbolu O(m^2)
; a slozitost zakodovani zpravy O(m^2 n)
