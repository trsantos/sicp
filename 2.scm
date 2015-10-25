(define (runtime) (tms:clock (times)))

;;; Support

(define true #t)
(define false #f)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (square x) (* x x))

;;; Text

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

;;; 2.53

(list 'a 'b 'c)
;; (a b c)
(list (list 'george))
;; ((george))
(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))
(cddr '((x1 x2) (y1 y2)))
;; (y1 y2)
(pair? (car '(a short list)))
;; #f
(memq 'red '((red shoes) (blue socks)))
;; #f
(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;;; 2.54

(define (new-equal? a b)
  (or (and (symbol? a)
	   (symbol? b)
	   (eq? a b))
      (and (pair? a)
	   (pair? b)
	   (new-equal? (car a) (car b))
	   (new-equal? (cdr a) (cdr b)))
      (and (null? a) (null? b))))

;;; 2.55

(car ''abracadabra)

;;; Text

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
	 (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2))
	 (+ a1 a2))
	(else (list '+ a1 a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
;; 1
(deriv '(* x y) 'x)
;; y
(deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* x y) (* y (+ x 3)))

;;; 2.56

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
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (make-sum (exponent exp) -1)))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type: DERIV" exp))))

(define (make-exponentiation b n)
  (cond ((=number? n 0) 1)
	((=number? n 1) b)
	(else (list '** b n))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(deriv '(** x 5) 'x)
;; (* 5 (** x 4))

;;; 2.57

(define (augend s)
  (accumulate make-sum 0 (cddr s)))
(define (multiplicand p)
  (accumulate make-product 1 (cddr p)))

(deriv '(+ 1 2 5 6 x) 'x)
;; 1
(deriv '(* x y (+ x 3)) 'x)
;; (+ (* x y) (* y (+ x 3)))

;;; 2.58

;; a)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2))
	 (+ a1 a2))
	(else (list a1 '+ a2))))
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
;; 4

;; b)

					; MUITO DIFÍCIL :(

;;; Text

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

;;; 2.59

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

(union-set '(1 2 3 4) '(3 4 5 6))
;; (2 1 3 4 5 6)

;;; 2.60

(define (adjoin-set x set)
  (cons x set))

;;; Text

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1)
					  (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

;;; 2.61

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
	((= x (car set)) set)
	(else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 '(1 3 5 8))
;; (1 3 5 8)
(adjoin-set 4 '(1 3 5 8))
;; (1 3 4 5 8)
(adjoin-set 10 '(1 3 5 8))
;; (1 3 5 8 10)

;;; 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set (cdr set1) set2)))
		 ((< x2 x1)
		  (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '(1 2 3 4) '(1 2 5 6))
;; (1 2 3 4 5 6)
(union-set '(3 4 5) '(1 2 5 6))
;; (1 2 3 4 5 6)
(union-set '(8 9 10) '(1 2 5 6))
;; (1 2 5 6 8 9 10)

;;; Text

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
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
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;;; 2.63

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

;; a)

;; Os dois procedimentos produzem o mesmo resultado para qualquer árvore que
;; passarmos a eles. Quanto às árvores da Figura 2.16, obtemos uma lista
;; com os items das árvores em ordem crescente, todas as vezes.

;; b)

;; tree->list-1 é um procedimento de ordem O(n * log n), pois fazemos uso
;; do procedimento append para cada ramo esquerdo da árvore, ou seja,
;; chamamos append log n vezes. E isto para cada entrada da árvore, que é
;; visitada uma vez nos dois algoritmos, havendo portanto n chamadas a append.
;;
;; Já o procedimento tree->list-2, ele tem ordem O(n) pois, assim como o
;; primeiro, ele faz uma visita a cada entrada da árvore e, em cada uma
;; delas, é feita apenas uma chamada a cons, que é O(1).

;;; 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result
	       (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result
		   (partial-tree
		    (cdr non-left-elts)
		    right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts
		     (cdr right-result)))
		(cons (make-tree this-entry
				 left-tree
				 right-tree)
		      remaining-elts))))))))

;; a)

;; list->tree divide os elementos da lista em três partes: a entrada atual,
;; que é o elemento do meio da lista e as listas dos elementos à esquerda e à
;; direita do elemento do meio, que são menores e maiores que ele,
;; respectivamente. Nisso, as árvores parciais destas duas outras listas são
;; construídas recursivamente para que, ao final do procedimento, possamos
;; construir uma árvore em que o elemento do meio seja a raiz, e as outras
;; árvores parciais postas à esquerda e à direita da raiz.

(list->tree '(1 3 5 7 9 11))
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;; b)

;; Como o procedimento visita cada elemento da lista apenas uma vez e em cada
;; visita há apenas uma chamada a cons, o procedimento opera em ordem
;; proporcional ao tamanho da lista, ou seja, é O(n).

;;; 2.65

(define (union-set-l set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set-l (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set-l (cdr set1) set2)))
		 ((< x2 x1)
		  (cons x2 (union-set-l set1 (cdr set2)))))))))

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (union-set-l list1 list2))))

(define (intersection-set-l set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set-l (cdr set1)
					    (cdr set2))))
	      ((< x1 x2)
	       (intersection-set-l (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set-l set1 (cdr set2)))))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (intersection-set-l list1 list2))))

;;; Text

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else (lookup given-key (cdr set-of-records)))))
(define key +) ;; dummy value for key...

;;; 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (entry set-of-records))) true)
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records)))
	 (lookup given-key (right-branch set-of-records)))))

;;; Text

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch  tree) (car  tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

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

;;; 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree
		    (make-leaf 'D 1)
		    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;; (A D A B B C A)

;;; 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (cond ((element-of-set? symbol (symbols (left-branch tree)))
	     (cons 0 (encode-symbol symbol (left-branch tree))))
	    ((element-of-set? symbol (symbols (right-branch tree)))
	     (cons 1 (encode-symbol symbol (right-branch tree))))
	    (else (error "bad symbol: ENCODE-SYMBOL" symbol)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(encode '(A D A B B C A) sample-tree)
;; (0 1 1 0 0 1 0 1 0 1 1 1 0)

(equal? (encode '(A D A B B C A) sample-tree)
	sample-message)
;; #t

;;; 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set (make-code-tree (car leaf-set)
				   (cadr leaf-set))
		   (cddr leaf-set)))))

;;; 2.70

(define rock-tree (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1)
					   (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define encoded-rock-song (encode '(GET A JOB
					SHA NA NA NA NA NA NA NA NA
					GET A JOB
					SHA NA NA NA NA NA NA NA NA
					WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
					SHA BOOM)
				  rock-tree))

;; São necessários 84 bits para codificar a mensagem. Se usássemos um
;; código de comprimento fixo para cada símbolo, precisaríamos de
;; 108 bits para codificar a mesma mensagem.

;;; 2.71

;; Item mais frequente: 1 bit
;; Item menos frequente: 2^(n-1) bits

;;; 2.72

;; Para codificar o mais frequente: O(N)
;; Para codificar o menos frequente: O(N²)

;;; Text

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

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

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

;;; From 3.3.3

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record (cdr record) false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define (get op type) ((operation-table 'lookup-proc) op type))
(define (put op type item) ((operation-table 'insert-proc!) op type item))

;;; Back to 2.4.3

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
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)

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
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types: APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp))
	       (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a) deriv é agora um operador genérico que irá chamar procedimentos
;; diferentes dependendo do tipo de expressão algébrica contida em exp
;; (soma, produto, etc). No entanto, number? e variable? não podem ser
;; assimilados no dispatch porque eles dizem respeito a outras
;; categorias de expressões, para as quais o prodedimento operator
;; não faria sentido.

;; b) e c)

(define (install-deriv-package)
  ;; internal procedures
  (define (=number? exp num) (and (number? exp) (= exp num)))
  ;; sum
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2))
	   (+ a1 a2))
	  (else (list '+ a1 a2))))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  ;; product
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (list '* m1 m2))))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (deriv-product exp var)
    (make-sum (make-product
	       (multiplier exp)
	       (deriv (multiplicand exp) var))
	      (make-product
	       (deriv (multiplier exp) var)
	       (multiplicand exp))))
  ;; exponentiation
  (define (make-exponentiation b n)
    (cond ((=number? n 0) 1)
	  ((=number? n 1) b)
	  (else (list '** b n))))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  (define (deriv-exponentiation exp var)
    (make-product
     (make-product (exponent exp)
		   (make-exponentiation (base exp)
					(make-sum (exponent exp) -1)))
     (deriv (base exp) var)))

  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)
(install-deriv-package)

;; d) Teríamos que mudar a interface do sistema de diferenciação (e de todos
;; os outros pacotes) para acomodar a mudança na ordem dos argumentos de
;; put.

;;; 2.74

;; a)

(define (get-record name file)
  ((get 'get-record (type-tag file)) name (contents file)))

;; Cada divisão precisa adicionar seu nome como tag do arquivo de registros.
;; Algo como: '(div-1 (div-1 ("Mike" (salary 5) (age 10)))
;;                    (div-1 ("John" (salary 10) (age 5))))

;; b)

(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

;; Cada divisão também precisa adicionar seu nome em cada registro de seus
;; empregados. Há uma redundância aqui...

;; c)

(define (find-employee-record name files)
  (if (null? files)
      '()
      (let ((record (get-record name (car files))))
	(if (null? record)
	    (find-employee-record name (cdr files))
	    record))))

;; d) Cada empresa adquirida precisará instalar o seu próprio pacote
;; contendo suas versões de get-record, get-salary, etc. Os procedimentos
;; genéricos que escrevemos não irão precisar de modificações.

;;; Text

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
	  ((eq? op 'imag-part) y)
	  ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
	  ((eq? op 'angle) (atan y x))
	  (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; (define (apply-generic op arg) (arg op))

;;; 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;; 2.76

;; Copiado do Ivan Ivanov

;; Adding new operations to a system that uses generic operations with 
;; explicit dispatch can be done without any changes to existing code. 
;; However, adding a new type would require adding extra code to all 
;; procedures (operations) that can be performed on that type.

;; In the case of a system implemented in data-directed style, both adding
;; new operations and new types can be done without changes to existing code.

;; A system implemented in message-passing style allows for addition of new
;; types without changes to existing code. However, adding a new operation
;; would require adding code to all existing types which have to support
;; this operation.

;; For a system that will require a frequent addition of new types both
;; data-directed style and message-passing style could be adequate choices.

;; On the other hand, for a system that will require frequent addition of
;; new operations, either data-directed style or generic operations with
;; explicit dispatch might be considered.

;;; Text

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

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
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) zero?)
  (put 'raise '(scheme-number)
       (lambda (x) (if (exact-integer? x)
		       (make-rational x 1)
		       (make-complex-from-real-imag x 0))))
  (put 'project '(scheme-number)
       (lambda (x) (inexact->exact (round x))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)
(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
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
  (define (equ? x y)
    (and (= (numer x) (numer y))
	 (= (denom x) (denom y))))
  (define (=zero? x) (zero? (numer x)))
  ;; interface to rest of system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational)
       (lambda (x) (exact->inexact (/ (numer x) (denom x)))))
  (put 'project '(rational)
       (lambda (x) (inexact->exact (round (exact->inexact (/ (numer x) (denom x)))))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

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
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (zero? (real-part z))
	 (zero? (imag-part z))))
  ;; interface to rest of system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'project '(complex)
       (lambda (x) (real-part x)))
  ;; from 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; 2.77

(define z (make-complex-from-real-imag 3 4))
;; (complex rectangular 3 . 4)

(magnitude '(complex rectangular 3 . 4))
(apply-generic 'magnitude '(complex rectangular 3 . 4))
(magnitude '(rectangular 3 . 4))
(apply-generic 'magnitude '(rectangular 3 . 4))
;; Abaixo, magnitude é interno ao pacote rectangular.
;; (apply magnitude '(3 . 4))

;;; 2.78

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum: CONTENTS" datum))))

;;; 2.79

(define (equ? x y)
  (apply-generic 'equ? x y))

;;; 2.80

(define (=zero? x)
  (apply-generic '=zero? x))

;;; Text

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (put-coercion type1 type2 item)
  (put type1 type2 item))
(define (get-coercion type1 type2)
  (get type1 type2))

(put-coercion 'scheme-number 
	      'complex
	      scheme-number->complex)

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
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else (error "No method for these types"
				     (list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))

;;; 2.81

;; a) Se invocarmos apply-generic tentando aplicar exp a dois números
;; complexos, mas exp estiver definido apenas para dois números ordinários,
;; então o método de coerção será utilizado e, como Louis adicionou coerções
;; de complexo para complexo, o resultado será uma nova chamada a
;; apply-generic com exatamente os mesmos argumentos. Um loop infinito.

;; b) Louis está errado, pois apply-generic funciona do jeito que está definido.
;; Se não houver um determinado operador definido para dois argumentos
;; complexos, por exemplo, então tentaremos encontrar alguma coerção válida.
;; Mas a tentativa irá falhar, pois não haverá uma coerção complex->complex.
;; Então simplesmente mostraremos o erro e apply-generic terminará.

;; c)

(define (apply-generic op . args)
  (define (error-message type-tags)
    (error "No method for these types"
	   (list op type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq? type1 type2)
		    (error-message type-tags)
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else (error-message type-tags))))))
	      (error-message type-tags))))))

;;; 2.82

(define (apply-generic op . args)
  (define (coerce-list-to-type lst type)
    (if (null? lst)
	'()
	(let ((coercion (get-coercion (type-tag (car lst)) type)))
	  (cons (if coercion
		    (coercion (car lst))
		    (car lst))
		(coerce-list-to-type (cdr lst) type)))))

  (define (apply-generic-coerced lst)
    (if (null? lst)
	(error "No method for these types: APPLY-GENERIC")
	(let ((coerced-list (coerce-list-to-type args (type-tag (car lst)))))
	  (let ((type-tags (map type-tag coerced-list)))
	    (let ((proc (get op type-tags)))
	      (if proc
		  (apply proc (map contents coerced-list))
		  (apply-generic-coerced (cdr lst))))))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (apply-generic-coerced args)))))

;; Essa versão de apply-generic não funciona quando, por exemplo, precisamos
;; de um operador para os tipos t1 e t2, e t1 não pode ser convertido para t2
;; nem t2 para t1, mas os dois tipos podem ser convertidos para um terceiro
;; t3, para o qual o operador existe. Nesse caso, essa conversão não irá
;; ocorrer.

;;; 2.83

(define (raise x) (apply-generic 'raise x))

;;; 2.84

(define (tower-level x)
  (let ((type (type-tag x)))
    (cond ((eq? type 'rational) 1)
	  ((eq? type 'complex) 3)
	  (else (let ((n (contents x)))
		  (if (exact-integer? n)
		      0
		      2))))))

(define (raise-to level x)
  (if (= level (tower-level x))
      x
      (raise-to level (raise x))))

;; Versão para dois argumentos

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((a1 (car args))
		    (a2 (cadr args)))
		(let ((l1 (tower-level a1))
		      (l2 (tower-level a2)))
		  (cond ((< l1 l2) (apply-generic op (raise a1) a2))
			((> l1 l2) (apply-generic op a1 (raise a2)))
			(else (apply-generic op (raise a1) (raise a2))))))
	      (error "No method for these types: APPLY-GENERIC" (list op type-tags)))))))

;; Versões para qualquer número de argumentos. A segunda não funciona
;; por causa do let? Acho que raised-args não tem validade após a
;; chamada recursiva.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (let ((levels (map tower-level args)))
	    (let ((max-level (apply max levels)))
	      (let ((raised-args (map (lambda (x) (raise-to max-level x)) args)))
		(let ((type-tags (map type-tag raised-args)))
		  (let ((proc (get op type-tags)))
		    (if proc
			(apply proc (map contents raised-args))
			(error "No method for these types: APPLY-GENERIC" (list op type-tags))))))))))))

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;; 	  (apply proc (map contents args))
;; 	  (let ((levels (map tower-level args)))
;; 	    (if (apply = levels)
;; 		(error "No method for these types: APPLY-GENERIC" (list op type-tags))
;; 		(let ((max-level (apply max levels)))
;; 		  (let ((raised-args (map (lambda (x) (raise-to max-level x)) args)))
;; 		    (apply-generic op raised-args)))))))))

;;; 2.85

(define (project x) (apply-generic 'project x))

(define (drop x)
  (if (and (equ? (project x) x)
	   (not (= (tower-level x) 0)))
      (drop (project x))
      x))

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;; 	  (drop (apply proc (map contents args)))
;; 	  (let ((levels (map tower-level args)))
;; 	    (let ((max-level (apply max levels)))
;; 	      (let ((raised-args (map (lambda (x) (raise-to max-level x)) args)))
;; 		(let ((type-tags (map type-tag raised-args)))
;; 		  (let ((proc (get op type-tags)))
;; 		    (if proc
;; 			(drop (apply proc (map contents raised-args)))
;; 			(error "No method for these types: APPLY-GENERIC" (list op type-tags))))))))))))
;;
;; (project 5)

;;; 2.5.3

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1) (term-list p2)))
	(error "Polys not in same var: ADD-POLY" (list p1 p2))))
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

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1) (term-list p2)))
	(error "Polys not in same var: MUL-POLY" (list p1 p2))))
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

  (define (terms-zero? terms)
    (if (empty-termlist? terms)
	#t
	(and (=zero? (coeff (first-term terms)))
	     (terms-zero? (rest-terms terms)))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) (lambda (p) (terms-zero? (term-list p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; Não fiz o resto dos exercícios deste capítulo
