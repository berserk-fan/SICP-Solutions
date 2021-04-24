(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (sq x)
  (* x x))

(define (sqrt x)
  (define precision 0.00001)
  (define (good-enought y)
    (< (abs (- (sq y) x)) precision))
  (define (improve y)
    (/ (+ y (/ x y)) 2))
  (define (try y)
    (if (good-enought y)
      y
      (try (improve y))))
  (try 1))

(define (average x y)
  (/ (+ x y) 2)
)

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (mid-segment s)
  (define start (start-segment s))
  (define end (end-segment s))
  (define xe (x-point end))
  (define ye (y-point end))
  (define xs (x-point start))
  (define ys (y-point start))
  (make-point (average xe xs) (average ye ys)))

(define (length-segment s)
  (define start (start-segment s))
  (define end (end-segment s))
  (define xe (x-point end))
  (define ye (y-point end))
  (define xs (x-point start))
  (define ys (y-point start))
  (sqrt (+ (sq (- xs xe)) (sq (- ys ye)))))

(define (print-segment s)
  (print-point (start-segment s))
  (print-point (end-segment s)))

(define (make-rectangle side1 side2)
  (define width (min (side1) (side2)))
  (define height (max (side1) (side2)))
  (cons width height)
)

(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r) (height-rectangle r))))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

(define (width-rectangle r)
  (car r))

(define (height-rectangle r)
  (cdr r))
