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

;;; Text

