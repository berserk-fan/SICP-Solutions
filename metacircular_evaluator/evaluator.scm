(define (apply-in-underlying-scheme proc args) (apply proc args))

(define modules '())
(define (add-module module)
  (set! modules (cons module modules)))
(define (make-module predicate handler) (list predicate handler))
(define (predicate module) (car module))
(define (handler module) (cadr module))


(define (eval exp env)
  (define (helper left-modules)
    (if (null? left-modules)
	(error "Unknown expression type -- EVAL" exp)
	(let* ((cur-module (car left-modules))
	       (cur-predicate (predicate cur-module))
	       (cur-handler (handler cur-module)))
	  (if (cur-predicate exp)
	      (cur-handler exp env)
	      (helper (cdr left-modules))))))
  (helper modules))


;; commons

(define (eval-sequence exps env)
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (identity x) x)

;;makers
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-begin seq) (cons 'begin seq))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let bindings body) (append (list 'let bindings) body))
(define (make-assignment var val) (list 'set! var val))

;;primitives

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list 'display display)
	(list '= =)
	(list '+ +)
	(list '- -)
	(list 'map map)
	(list 'list list)
        ))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; REPL

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;environment

(define (enclosing-environment env) (cdr env))
(define (first-environment env) (list (first-frame env)))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (if (= (length variables) 0)
      '()
      (cons (list (car variables)
		  (car values))
	    (make-frame (cdr variables)
			(cdr values)))))

(define (add-binding-to-frame! var val frame)
  (let ((old-cdr (cdr frame))
	(old-car (car frame)))
    (begin (set-car! frame (list var val))
	   (set-cdr! frame (cons old-car old-cdr)))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (binding-key binding) (car binding))
(define (binding-value binding) (cadr binding))
(define (bind-value value binding) (set-cdr! binding (list value)))

(define (find-binding key env)
  (cond ((eq? env the-empty-environment) #f)
	((assv key (first-frame env)) => identity)
	(else (find-binding key (enclosing-environment env)))))
(define (find-binding! key env)
  (let ((binding (find-binding key env)))
    (if binding binding (error "Unbound variable" key))))

(define (lookup-variable-value var env)
  (let ((val (binding-value (find-binding! var env))))
    (if (eq? '*unassigned* val)
	(error "Unassigned variable" var)
	val)))

(define (set-variable-value! var val env)
  (bind-value val (find-binding! var env)))

(define (define-variable! var val env)
  (let ((binding (find-binding var (first-environment env))))
    (if binding
        (bind-value val binding)
	(add-binding-to-frame! var val (first-frame env)))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;;procedures

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;modules

(define (setup-standard-modules)
  (begin
    (setup-assignment-module)
    (setup-apply-module)
    (setup-cond-module)
    (setup-begin-module)
    (setup-quotation-module)
    (setup-if-module)
    (setup-let-module)
    (setup-lambda-module)
    (setup-basics-module)))

(define (setup-cond-module) ;;st
  (define (cond? exp) (tagged-list? exp 'cond))
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (special-syntax? clause) (eq? (quote (cadadr clause)) '=>))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause)
    (if (special-syntax? clause)
	('((caddr clause) (car clause)))
	(cdr clause)))
  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))


  (define (sequence->exp seq)
    (cond ((null? seq) seq)
	  ((last-exp? seq) (first-exp seq))
	  (else (make-begin seq))))

  (define (expand-clauses clauses)
    (if (null? clauses)
	'false                          ; no else clause
	(let ((first (car clauses))
	      (rest (cdr clauses)))
	  (if (cond-else-clause? first)
	      (if (null? rest)
		  (sequence->exp (cond-actions first))
		  (error "ELSE clause isn't last -- COND->IF"
			 clauses))
	      (make-if (cond-predicate first)
		       (sequence->exp (cond-actions first))
		       (expand-clauses rest))))))

  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  
  (add-module (make-module cond? eval-cond)))

(define (setup-begin-module) ;st
  (define (begin? exp) (tagged-list? exp 'begin))
  (define (begin-actions exp) (cdr exp))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  
  (add-module (make-module begin? eval-begin)))

(define (setup-assignment-module)
  (define (assignment? exp)
    (tagged-list? exp 'set!))
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
			 (eval (assignment-value exp) env)
			 env)
    'ok)
  (add-module (make-module assignment? eval-assignment)))

(define (setup-quotation-module)
  (define (quoted? exp)
    (tagged-list? exp 'quote))
  
  (define (text-of-quotation exp env) (cadr exp))

  (add-module (make-module quoted? text-of-quotation)))

(define (setup-apply-module)
  (define (application? exp) (tagged-list? exp 'call))
  (define (operator exp) (cadr exp))
  (define (operands exp) (cddr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
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
  
  (define (eval-application exp env)
    (apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
  
  (add-module (make-module application? eval-application)))

(define (setup-basics-module)
  (define (self-evaluating? exp)
    (cond ((number? exp) true)
	  ((string? exp) true)
	  (else false)))

  (define (variable? exp) (symbol? exp))
  (begin
    (add-module (make-module self-evaluating?
			     (lambda (x y) x)))
    (add-module (make-module variable?
			     (lambda (exp env) (lookup-variable-value exp env))))))


(define (setup-if-module)
  (define (true? x)
    (not (eq? x false)))
  (define (false? x)
    (eq? x false))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
	(eval (if-consequent exp) env)
	(eval (if-alternative exp) env)))
  
  (define (if? exp) (tagged-list? exp 'if))
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
	(cadddr exp)
	'false))
  (add-module (make-module if? eval-if)))

(define (setup-lambda-module)
  (define (definition? exp)
    (tagged-list? exp 'define))

  (define (definition-variable exp)
    (if (symbol? (cadr exp))
	(cadr exp)
	(caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
	(caddr exp)
	(make-lambda (cdadr exp)   ; formal parameters
		     (cddr exp)))) ; body

  
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)

  (add-module (make-module definition? eval-definition))
  
  (define (lambda? exp) (tagged-list? exp 'lambda))
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))

  (define (make-procedure parameters body env)
    (define (scan-out-defines body)
      (receive (defines rest) (partition definition? body)
	(let* ((bindings (map (lambda (def)
				(list (definition-variable def) (list 'quote '*unassigned*)))
			      defines))
	       (setters (map (lambda (def)
			       (make-assignment (definition-variable def)
						(definition-value def)))
			     defines))
	       (res (if (null? defines)
			body
			(list (make-let bindings (append setters rest))))))
	  res)))
    (list 'procedure parameters (scan-out-defines body) env))

  
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))
  
  (add-module (make-module lambda? eval-lambda)))

(define (setup-boolops-module)
  (define (and? exp) (tagged-list? exp 'and))
  
  (define (or? exp) (tagged-list? exp 'or))

  (define (eval-and exp env)
    (define (helper rest)
      (cond ((null? rest) true)
	    ((eval (car rest) env) (helper (cdr rest)))
	    (else false)))
    (if (< (length exp) 2)
	(error "Missing operands to 'and' expr")
	(helper (cdr exp))))
  
  (define (eval-or exp env)
    (define (helper rest)
      (cond ((null? rest) false)
	    ((eval (car rest) env) true)
	    (else (helper (cdr rest)))))
    (if (< (length exp) 2)
	(error "Missing operands to 'or' expr")
	(helper (cdr exp))))

  (add-module (make-module or? eval-or))
  (add-module (make-module and? eval-and))
  )

(define (setup-let-module)
  (define (let? exp)
    (tagged-list? exp 'let))
  
  (define (bindings->vars bindings)
    (if (null? bindings)
	'()
	(cons (caar bindings) (bindings->vars (cdr bindings)))))
  
  (define (bindings->exps bindings)
    (if (null? bindings)
	'()
	(cons (cadar bindings) (bindings->exps (cdr bindings)))))
  
  (define (let->combination exp)
    (let* ((bindings (cadr exp))
	   (body (cddr exp)))
      (append (list 'call	     
		    (make-lambda (bindings->vars bindings) body))
	      (bindings->exps bindings))))

  (define (eval-let let env) (eval (let->combination let) env))

  
  (add-module (make-module let? eval-let))
  
  (define (let*? exp) (tagged-list? exp 'let*))
  (define (let*->nested-lets exp)
    (let ((body (caddr exp)))
      (define (helper bindings)
	(let ((left-bindigs (cdr bindings)))
	  (if (null? left-bindings)
	      body
	      (make-let (list (car bindings))
			(helper rest-bindings)))))
      (helper (cadr exp))))
  (define (eval-let* exp env) (eval (let*->nested-lets exp) env))
  (add-module (make-module let*? eval-let*))
  )

(setup-boolops-module)
(setup-standard-modules)
(define the-global-environment (setup-environment))
(define (evalG expr) (eval expr the-global-environment))

'ok

;;4.14 because represantations of procedures are different
;; Exercise 4.15.  Given a one-argument procedure p and an object a, p is said to ``halt'' on a if evaluating the expression (p a) returns a value (as opposed to terminating with an error message or running forever). Show that it is impossible to write a procedure halts? that correctly determines whether p halts on a for any procedure p and object a. Use the following reasoning: If you had such a procedure halts?, you could implement the following program:

;; (define (run-forever) (run-forever))

;; (define (try p)
;;   (if (halts? p p)
;;       (run-forever)
;;       'halted))
;;
;; (try try): if halts then (halts? p p) returns true then (try try) (run-forever)
;;            if forever then (halts? p p) = false (try try) halts
