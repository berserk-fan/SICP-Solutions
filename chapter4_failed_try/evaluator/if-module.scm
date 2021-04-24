(define (if-module eval put-evaluator get-op put-op)
  (define (if? exp) (tagged-list? exp 'if))
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
	(cadddr exp)
	'false))
  
  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
	(eval (if-consequent exp) env)
	(eval (if-alternative exp) env)))
  (put-op 'if make-if make-if)
  (put-evaluator 'if eval-if))

if-module
