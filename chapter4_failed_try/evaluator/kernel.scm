(define (init put-op)
  ;; procedures
  (define (primitive-procedure? proc)
     (tagged-list? proc 'primitive))
   
  (define (primitive-implementation proc)
    (cadr proc))
  
  (define primitive-procedures
    (list (list 'car car)
	  (list 'cdr cdr)
	  (list 'cons cons)
	  (list 'null? null?)
	  (list '+ +)))
  
  (define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
	 primitive-procedures))

  (define (primitive-procedure-names)
    (map car primitive-procedures))

  (define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args))

  (define (make-procedure parameters body env)
    (list 'procedure parameters body env))
  (define (compound-procedure? p)
    (tagged-list? p 'procedure))
  (define (procedure-parameters p) (cadr p))
  (define (procedure-body p) (caddr p))
  (define (procedure-environment p) (cadddr p))


  (put-op 'kernel 'primitive-procedure? primitive-procedure?)
  (put-op 'kernel 'apply-primitive-procedure apply-primitive-procedure)
  (put-op 'kernel 'make-procedure make-procedure)
  (put-op 'kernel 'compound-procedure? compound-procedure?)
  (put-op 'kernel 'procedure-parameters procedure-parameters)
  (put-op 'kernel 'procedure-environment procedure-environment)


  ;;environment
  (define (enclosing-environment env) (cdr env))
  (define (first-frame env) (car env))
  (define the-empty-environment '())

  (define (make-frame variables values)
    (cons variables values))
  (define (frame-variables frame) (car frame))
  (define (frame-values frame) (cdr frame))
  (define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))

  (define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
	(cons (make-frame vars vals) base-env)
	(if (< (length vars) (length vals))
	    (error "Too many arguments supplied" vars vals)
	    (error "Too few arguments supplied" vars vals))))

  (define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
	(cond ((null? vars)
	       (env-loop (enclosing-environment env)))
	      ((eq? var (car vars))
	       (car vals))
	      (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
	  (error "Unbound variable" var)
	  (let ((frame (first-frame env)))
	    (scan (frame-variables frame)
		  (frame-values frame)))))
    (env-loop env))

  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
	(cond ((null? vars)
	       (env-loop (enclosing-environment env)))
	      ((eq? var (car vars))
	       (set-car! vals val))
	      (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
	  (error "Unbound variable -- SET!" var)
	  (let ((frame (first-frame env)))
	    (scan (frame-variables frame)
		  (frame-values frame)))))
    (env-loop env))

  (define (define-variable! var val env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
	(cond ((null? vars)
	       (add-binding-to-frame! var val frame))
	      ((eq? var (car vars))
	       (set-car! vals val))
	      (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
	    (frame-values frame))))

  (define (setup-environment)
    (let ((initial-env
	   (extend-environment (primitive-procedure-names)
			       (primitive-procedure-objects)
			       the-empty-environment))))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env)

  (put-op 'kernel 'lookup-variable-value lookup-variable-value)
  (put-op 'kernel 'setup-environment setup-environment)
  
  (define (true? x)
    (not (eq? x false)))
  (define (false? x)
    (eq? x false))

init
