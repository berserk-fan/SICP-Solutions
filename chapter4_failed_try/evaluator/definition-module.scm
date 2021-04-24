;;definition
(define (definition-module eval put-evaluator get-op put-op)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
	(cadr exp)
	(caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
	(caddr exp)
	((get-op 'lambda 'make-lambda)
	 (cdadr exp)   ; formal parameters
	 (cddr exp)))) ; body
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)
  (put-evaluator 'define eval-definition))

definition-module
