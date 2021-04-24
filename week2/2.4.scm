;;2.73
;;Exercise 2.73.  Section 2.3.2 described a program that performs symbolic differentiation:
;;
;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;            (make-product (multiplier exp)
;;                          (deriv (multiplicand exp) var))
;;            (make-product (deriv (multiplier exp) var)
;;                          (multiplicand exp))))
;;         <more rules can be added here>
;;         (else (error "unknown expression type -- DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of the expression to be differentiated. In this situation the ``type tag'' of the datum is the algebraic operator symbol (such as +) and the operation being performed is deriv. We can transform this program into data-directed style by rewriting the basic derivative procedure as

;; (define (deriv exp var)
;;    (cond ((number? exp) 0)
;;          ((variable? exp) (if (same-variable? exp var) 1 0))
;;          (else ((get 'deriv (operator exp)) (operands exp)
;;                                             var))))
;; (define (operator exp) (car exp))
;; (define (operands exp) (cdr exp))

;; a.  Explain what was done above. Why can't we assimilate the predicates number? and same-variable? into the data-directed dispatch?

;; b.  Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

;; c.  Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56), and install it in this data-directed system.

;; d.  In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like

;; ((get (operator exp) 'deriv) (operands exp) var)

;; What corresponding changes to the derivative system are required?
;; Expect variable represented like '(id_x, x)
;; get 'deriv 
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))
;; a) assume number is eliminated then we have to represent numbers as ('number 1) then we make call: (get 'deriv 'number)
;;we get (lambda (x var) 0).
;; the same can be done with "variable?" part
;; (get 'deriv 'variable) -> (lambda (x var) (if (eq? x var)
;;1 0))

;; b)
(define (deriv-sum elements var)
  (make-sum (map (lambda (x) (deriv x var))
		 elements)))
(set 'deriv 'sum 'deriv-sum)

(define (deriv-product elements var)
  (make-sum
   ;; approximate code cdr should be unapplied and passed as
   ;; multiple parameters
   (make-product (deriv (car elements) var)
		 (cdr elements))
   (make-product (deriv-product (cdr elemetns) var)
		 (car elements))
   ))
(set 'deriv 'product 'deriv-product)

;;c)
(define (deriv-exp operands var)
  (make-product (deriv (cadr operands) var)
		(make-exp operands)
		(make-ln (caadr operands))))


;;d)
;; it is required to change setting up statements
j
