;;Ex 4.1
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((first-elem (eval (first-operand exps) env))
	     (second-elem (list-of-values (rest-operands exps) env)))
	(cons  first-elem second-elem))))
;;right to left similar

;;Ex 4.2
;;a. define will be treated as procedure
;;


;;Ex.4.3
;;self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else eval-generic exp env)))

(define (eval-gerneric exp env)
  (let ((exp-evaluator (get-evaluator (car exp))))
    (if exp-evaluator
	(exp-evaluator exp env)
	(error "Unknown expression type -- EVAL" exp)))
