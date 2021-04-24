(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insifficient funds"))

(define (make-withdrawal initial-balance)
  (lambda (amount)
    (if (>= initial-balance amount)
      (begin (set! initial-balance (- initial-balance amount))
	     initial-balance)
      "Insifficient funds")))

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

;;Ex 3.1
(define (make-accumulator acc)
  (lambda (addent)
    (begin (set! acc (+ acc addent))
	   acc)))

;;Ex 3.2
(define (make-monitored f)
  (let ((calls 0))
    (define (apply input)
      (begin (set! calls (+ calls 1))
	     (f input)))
    (define (how-many-calls?) calls)
    (define (reset-calls) (begin (set! calls 0)
				 "count was reset"))
    (define (dispatch el)
      (cond ((eq? el 'how-many-calls?) calls)
	    ((eq? el 'reset-count) (reset-calls))
	    (else (apply el))))
    dispatch))

;;Ex 3.3, 3.4
(define (call-cops)
  (display "Cops was called"))

(define (make-protected f real-password max-wrong-attempts)
  (let ((wrong-attempts 0))
    (lambda (arg1 password)
    (if (eq? password real-password) (f arg1)
	(begin (set! wrong-attempts (+ wrong-attempts 1))
	       (if (>=  wrong-attempts max-wrong-attempts) (call-cops))
	       "Incorrect password")))))

(define (make-account balance password)
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
  (make-protected dispatch password 7))

(define protAcc (make-account 100 'qwerty))

;;Ex 3.7
(define (make-joint acc owner-password new-owner-password)
  (define (dispatch arg)
    (acc owner-password))
  
  (make-protected_dispatch new-owner-password 7))

(define f-local 1)
(define (f x)
  (begin (set! f-local (* f-local x))
	 f-local))

(display "\n First return value\n")
(display (+ (f 0) (f 1)))
(display "\n")

(set! f-local 1)

(display "Second return value:\n")
(display (+ (f 1) (f 0)))
(display "\n")

;;; Part: 3.2 The Environment Model of Evaluation

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;;Exercise 3.9
;; first case, environment with n = ... for each call to factorial
;; second case, 1 environment for factorial. 6 environments for
;; fact-iter
