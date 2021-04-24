(define (quotations-module eval put-evaluator get-op put-op)
  (define (text-of-quotation exp env) (cadr exp))
  (put-evaluator 'quote text-of-quotation))

quotations-module
