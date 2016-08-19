(define (runtime) (tms:clock (times)))

;;; CODE TO SUPPORT CHAPTER 3 OF STRUCTURE AND INTERPRETATION OF
;;;  COMPUTER PROGRAMS
;;; NB. This code is *not* from the book

;;; In addition to code supplied here
;;;**For 3.4, might want parallel-execute as implemented for MIT Scheme
;;;**For 3.5, need stream special forms, which are not in Standard Scheme


;;For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))


;;For Section 3.3.4, used by and-gate
;;Note: logical-and should test for valid signals, as logical-not does
(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))


;;For Section 3.5 -- useful for looking at finite amounts of infinite streams
;;Print the first n elements of the stream s.
;;One version prints on one line, one on separate lines

;; (define (print-n s n)
;;   (if (> n 0)
;;       (begin (display (stream-car s))
;;              (display ",")
;;              (print-n (stream-cdr s) (- n 1)))))

;; (define (print-n s n)
;;   (if (> n 0)
;;       (begin (newline)
;; 	     (display (stream-car s))
;;              (print-n (stream-cdr s) (- n 1)))))


;;For Section 3.5.2, to check power series (exercises 3.59-3.62)
;;Evaluate and accumulate n terms of the series s at the given x
;;Uses horner-eval from ex 2.34
;; 
;; (define (eval-power-series s x n)
;;   (horner-eval x (first-n-of-series s n)))
;; (define (first-n-of-series s n)
;;   (if (= n 0)
;;       '()
;;       (cons (stream-car s) (first-n-of-series (stream-cdr s) (- n 1)))))

;;; Text

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

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
	  (else (error "Unknown request: MAKE-ACCOUNT"
		       m))))
  dispatch)

;; (define acc (make-account 100))

;; (define acc2 (make-account 100))

;;; 3.1

(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ sum x))
    sum))

(define A (make-accumulator 5))
(A 10)
;; 15
(A 10)
;; 25

;;; 3.2

(define (make-monitored f)
  (let ((counter 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) counter)
	    ((eq? m 'reset-count) (set! counter 0))
	    (else
	     (set! counter (1+ counter))
	     (f m))))
    dispatch))

;;; 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password x) "Incorrect password")
  (define (dispatch pw m)
    (if (eq? pw password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request: MAKE-ACCOUNT"
			   m)))
	incorrect-password))
  dispatch)

;;; 3.4

(define (make-account balance password)
  (let ((incorrect-pw 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password x) "Incorrect password")
    (define (call-the-cops x) "Called the cops!")
    (define (dispatch pw m)
      (if (eq? pw password)
	  (begin (set! incorrect-pw 0)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       (else (error "Unknown request: MAKE-ACCOUNT"
				    m))))
	  (begin (set! incorrect-pw (1+ incorrect-pw))
		 (if (> incorrect-pw 7)
		     call-the-cops
		     incorrect-password))))
    dispatch))

;;; Text

(define random-init 1)
(define rand (let ((x random-init))
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

;;; 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (* (abs (- x2 x1)) (abs (- y2 y1)))
     (monte-carlo trials p)))

(define (unit-circle-test)
  (let ((x (random-in-range -1.0 1.0))
	(y (random-in-range -1.0 1.0)))
    (<= (+ (* x x) (* y y)) 1)))

;; (estimate-area unit-circle-test -1.0 1.0 -1.0 1.0 100000)
;; => 3.1322

;;; 3.6

(define rand
  (let ((x random-init))
    (define generate
      (lambda ()
	(set! x (rand-update x))
	x))
    (define (reset new-value)
      (set! x new-value))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
	    ((eq? m 'reset) reset)
	    (else (error "Unknown request: RAND" m))))
    dispatch))

;; Another version

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
	     (set! x (rand-update x))
	     x)
	    ((eq? m 'reset)
	     (lambda (new-value)
	       (set! x new-value)))
	    (else (error "Unknown request: RAND" m))))))

;;; Text

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

;;; 3.7

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (cond ((not (eq? pw password)) (error "Incorrect password"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint acc acc-password new-password)
  (lambda (pw m)
    (if (eq? pw new-password)
	(acc acc-password m)
	"Incorrect password")))

;;; 3.8

(define f
  (let ((x 2))
    (lambda (n)
      (begin (set! x (- x 1))
	     (* x n)))))

;; Solução do Eli Bendersky (não exatamente melhor que a minha)
(define f
  (let ((state 1))
    (lambda (n)
      (set! state (* state n))
      state)))

(+ (f 0) (f 1))
;; 0 se a expressão for avaliada da esquerda para a direita
;; 1 se da direita para a esquerda

;;; SECTION 3.2.1

(define (square x)
  (* x x))

(define square
  (lambda (x) (* x x)))

;;; SECTION 3.2.2

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;: (sum-of-squares (+ a 1) (* a 2))

;;; 3.9

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Feito num caderno

;;; SECTION 3.2.3

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;: (define W1 (make-withdraw 100))
;: (W1 50)

;: (define W2 (make-withdraw 100))

;;; 3.10

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; versão equivalente usando apenas lambdas
(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
	   (begin (set! balance (- balance amount))
		  balance)
	   "Insufficient funds")))
   initial-amount))

;: (define W1 (make-withdraw 100))
;: (W1 50)
;: (define W2 (make-withdraw 100))

;; Fiz os diagramas no caderno. Mas uma diferença entre as duas estruturas é que,
;; usando a definição de make-withdraw deste exercício, o valor de initial-amount
;; é preservado dentro dos objetos W1 e W2 e nós poderíamos utilizá-lo se assim o
;; desejássemos. Quer dizer, há um frame intermediário contendo
;; initial-amount entre o que contém balance e o global.
;;
;; Nota: Os diagramas feitos por Weiqun Zhang estão muito bons:
;; https://wqzhang.wordpress.com/2009/07/13/sicp-exercise-3-10/


;;;SECTION 3.2.4

(define (average x y)
  (/ (+ x y) 2))

;; same as in section 1.1.8
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;; EXERCISE 3.11

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
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;: (define acc (make-account 50))
;:
;: ((acc 'deposit) 40)
;: ((acc 'withdraw) 60)
;:
;: (define acc2 (make-account 100))

;; Desenhos no caderno... Preciso tirar fotos desses exercícios?
;;
;; O estado local de acc é armazenado no frame criado quando chamamos (make-account 50).
;; Neste frame, estão definidos balance, withdraw, deposit e dispatch, sendo os
;; três últimos ponteiros para objetos de funções em que o ambiente pai é o
;; próprio frame. acc será definido no ambiente global como que apontando para
;; dispatch definido no frame citado.
;;
;; Se definirmos outro objeto account acc2 no ambiente global, este também terá
;; seu próprio ambiente e balance estará definido localmente para acc2.
;; As outras definições (withdraw, deposit e dispatch) poderiam apontar para
;; os mesmos objetos de função criados quando definimos acc, sendo portanto
;; compartilhados. Isso não afetaria o estado local de acc nem de acc2, já
;; que o que houver de diferente entre eles estará definido em frames separados.

;;;;SECTION 3.3

;;;SECTION 3.3.1

(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))


;; EXERCISE 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;: (define x (list 'a 'b))
;: (define y (list 'c 'd))
;: (define z (append  x y))
;: z
;: (cdr x)
;:
;: (define w (append! x y))
;: w
;: (cdr x)
