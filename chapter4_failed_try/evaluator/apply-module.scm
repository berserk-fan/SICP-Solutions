(define (apply-module eval put-evaluator get-op put-op)
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
  (define primitive-procedure?
    (get-op 'kernel 'primitive-procedure?))
  (define apply-primitive-procedure
    (get-op 'kernel 'apply-primitive-procedure))
  (define procedure-body
    (get-op 'kernel 'procedure-body))
  (define compound-procedure?
    (get-op 'kernel 'compound-procedure?))
  (define procedure-parameters
    (get-op 'kernel 'procedure-parameters))
  (define procedure-environment
    (get-op 'kernel 'procedure-environment))
  (define extend-environment
    (get-op 'environment 'extend-environment))
  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
	  (else (eval (first-exp exps) env)
		(eval-sequence (rest-exps exps) env))))
  (define (list-of-values exps env)
    (if (no-operands? exps)
	'()
	(cons (eval (first-operand exps) env)
	      (list-of-values (rest-operands exps) env))))
  (define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
	   (apply-primitive-procedure procedure arguments))
	  ((compound-procedure? procedure)
	   (eval-sequence
	    (procedure-body procedure)
	    (extend-environment
	     (procedure-parameters procedure)
	     arguments
	     (procedure-environment procedure))))
	  (else
	   (error
	    "Unknown procedure type -- APPLY" procedure))))
  (define (eval-apply exp env)
    (apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
  (put-evaluator 'call eval-apply))

apply-module
