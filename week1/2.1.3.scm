(define (isEven x)
  (equal? (modulo x 2) 0))
(define (isOdd x)
  (not (isEven x)))

(define (pow x k)
  (if (equal? k 0) 1
    (if (equal? k 2) (* x x)
      (if (isOdd k) (* x (pow x (- k 1)))
        (pow (pow x (/ k 2)) 2)))))

(define (my-cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (power-of-n x n)
  (cond((not (equal? (modulo x n) 0)) 0)
       (else (+ 1 (power-of-n (/ x n) n)))))

(define (power-of-two x)
  (power-of-n x 2))

(define (power-of-three x)
  (power-of-n x 3))

(define (my-car c)
  (power-of-two c))

(define (my-cdr c)
  (power-of-three c))

(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f ((zero f) x)))))

(define two (lambda (f) (lambda (x) (f ((one f) x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (church-n n)
  (cond ((< n 1) zero)
        (else (add-1 (church-n (- n 1))))))

(define (church+ n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
(define (add-one x)
  (+ x 1))

(define (calc-church n) ((n add-one) 0))
