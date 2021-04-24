;;assignment
(define (assignment-module eval put-evaluator get-op put-op)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
			 (eval (assignment-value exp) env)
			 env)
    'ok)
  (put-evaluator 'set! eval-assignment))

assignment-module
