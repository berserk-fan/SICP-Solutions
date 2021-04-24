(define make-table (load "/Users/dmytriim/Documents/NNotes/SICP/chapter3/3.3.scm"))

(define evaluator-table (make-table equal?))

(define ops-table (make-table equal?))


(define (get-evaluator tag)
  (display (list "Get-evaluator" tag "\n"))
  ((evaluator-table 'lookup) (list tag)))

(define (put-evaluator tag evaluator)
  (display (list "Put-evaluatro" tag "\n"))
  ((evaluator-table 'insert!) (list tag) evaluator))

(define (get-op module op)
  (display (list "Get op" module op "\n"))
  ((ops-table 'lookup) (list module op)))

(define (put-op module op f)
  (display (list "Put-op" op "\n"))
  ((ops-table 'insert!) (list module op) f))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (load-module path)
  ((load path) eval put-evaluator get-op put-op))

(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/if-module.scm")
(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/apply-module.scm")
(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/assignment-module.scm")
(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/begin-module.scm")
(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/definition-module.scm")
(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/cond-module.scm")
(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/lambda-module.scm")
(load-module "/Users/dmytriim/Documents/NNotes/SICP/chapter4/evaluator/quotation-module.scm")


(define the-global-environment (setup-environment))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) ((get-op 'kernel 'lookup-variable-value) exp env))
        ((eval-generic exp env) )
	((pair? exp) ((get-evaluator 'call) exp env))
	(else (error "Eval error"))))


(define (eval-generic exp env)
  (let ((exp-evaluator (get-evaluator (car exp))))
    (if (not (false? exp-evaluator))
	(exp-evaluator exp env)
	false)))

(define (eval-g exp) (eval exp the-global-environment))
