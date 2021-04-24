(define (gcd a b)
  (if (equal? b 0)
    a
    (gcd b (modulo a b))
  )
)
(define (make-rat x  y)
  (define gcd_ (gcd x y))
  (cons (/ x gcd_) (/ y gcd_)))
(define num (lambda(rat) (car rat)))
(define denom (lambda(rat) (cdr rat)))
(define (print-rat rat)
  (display (num rat))
  (display "/")
  (display (denom rat))
)