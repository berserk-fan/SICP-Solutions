(define (make-interval a b) (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

(define (sub-interval x y)
  (add-interval x
                ((make-interval -(upper-bound y)
                                -(lower-bound y)))))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval-eff x y)
  (define x1 (lower-bound x))
  (define x2 (upper-bound x))
  (define y1 (lower-bound y))
  (define y2 (upper-bound y))
  (cond
    ((and (< x1 0) (< x2 0) (< y1 0) (< y2 0)) (make-interval (* x1 y1) (* x2 y)))
    ((and (< x1 0) (< x2 0) (< y1 0) (>= y2 0)) (make-interval (* x1 y2) (* x1 y1)))
    ((and (< x1 0) (< x2 0) (>= y1 0) (>= y2 0)) (make-interval (* x1 y2) (* x2 y1)))
    ((and (< x1 0) (>= x2 0) (< y1 0) (< y2 0)) (make-interval (* x2 y2) (* x1 y2)))
    ((and (< x1 0) (>= x2 0) (< y1 0) (>= y2 0)) (mul-interval x y))
    ((and (< x1 0) (>= x2 0) (>= y1 0) (>= y2 0)) (make-interval (* x1 y2) (* x2 y2)))
    ((and (>= x1 0) (>= x2 0) (< y1 0) (< y2 0)) (make-interval (* x1 y2) (* x2 y1)))
    ((and (>= x1 0) (>= x2 0) (< y1 0) (>= y2 0)) (make-interval (* x1 y1) (* x2 y2)))
    ((and (>= x1 0) (>= x2 0) (>= y1 0) (>= y2 0)) (make-interval (* x1 y1) (* x2 y2)))
  ))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
         (p2 (* (lower-bound x) (upper-bound y)))
         (p3 (* (upper-bound x) (lower-bound y)))
         (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (interval-contains-null? y)
  (or (equal? (upper-bound y) 0) (equal? (lower-bound y) 0)))

(define (div-interval x y)
  (cond ((interval-contains-null? y) (display "INTERVAL CONTAINS ZERO CAN'T DIVIDE"))
        (else (mul-interval x
                            (make-interval (/ 1.0 (upper-bound y))
                                           (/ 1.0 (lower-bound y)))))))

(define (make-center-percent c p)
  (make-interval (- c (* p c)) (+ c (* p c))))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
  (/ (width i) (center i)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
    (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
      (add-interval (div-interval one r1)
        (div-interval one r2)))))
