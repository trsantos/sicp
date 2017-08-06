;;; Text

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;;(define (abs x)
;;  (cond ((> x 0) x)
;;	((= x 0) 0)
;;	((< x 0) (- x ))))

;;(define (abs x)
;;  (cond ((< x 0) (- x))
;;	(else x)))

;;(define (abs x)
;;  (if (< x 0)
;;      (- x)
;;      x))

;;(define (>= x y) (or (> x y) (= x y)))

;;(define (>= x y) (not (< x y)))

;;; 1.1

10
;;10

(+ 5 3 4)
;;12

(- 9 1)
;;8

(/ 6 2)
;;3

(+ (* 2 4) (- 4 6))
;;6

(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;;19

(= a b)
;;#f

(if (and (> b a) (< b (* a b)))
    b
    a)
;;4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;;16

(+ 2 (if (> b a) b a))
;;6

(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))
;;16

;;; 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
;;-37/150

;;; 1.3

(define (sum-of-squares-greater-2 x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
	((and (> x y) (> z y)) (sum-of-squares x z))
	(else (sum-of-squares y z))))

;;; 1.4

;; The procedure a-plus-abs-b features an expression for evaluating which
;; procedure to apply to the arguments a and b. That is, if b > 0, then
;; the chosen procedure is +, otherwise it is -. That has the effect of adding
;; the absolute value of b to a.

;;; 1.5

(define (p) (p))
(define (test x y)
  (if (= x y) 0 y))
;;(test 0 (p))

;; If the interpreter uses applicative-order evaluation, then it would try to
;; evaluate the expression (p) before applying the procedure test to its
;; arguments. That will result in an infinite loop, since the definition of p
;; is circular.

;;(test 0 (p))
;;(test 0 (p))
;;...

;; Otherwise, when using normal-order evaluation, the interpreter would never
;; evaluate (p). Instead, the (p) expression would be carried on as is, but
;; since the x in test would be 0, the if predicate would turn out to be
;; (= 0 0), which of course evaluates to true and the just returns the
;; consequent expression 0, without ever coming to the need of evaluating (p).

;;(test 0 (p))
;;(if (= 0 0) 0 (p))
;;0

;;; Text

;; (define (sqrt-iter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))

;; (define (sqrt x)
;;   (sqrt-iter 1.0 x))

;;; 1.6

;; The program will go through a infinite loop, because the all arguments for
;; the new-if procedure (in particular the third one) need to be evaluated
;; before the applying of new-if.

;;; 1.7 

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))

;;; 1.8

(define (cbrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (cbrt-iter (cbrt-improve guess x) guess x)))

(define (cbrt-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt x)
  (cube-iter 1.0 0 x))

