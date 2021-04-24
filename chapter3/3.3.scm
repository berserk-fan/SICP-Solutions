;;3.12 null, c
;;3.13 a -> b -> c -> a, infinite loop
;;3.14 reverts list
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
;;Ex. 3.15 -> skip
;;Ex. 3.16
;; 3 - (a (b (c nil))),
;; infinite - cyclic a -> b -> c -> a,
;; 4 - 2 pairs in list cars points to third pair,
;; 7 - 1st points to 2nd and third, 2nd points to 3rd and 3rd, 3rd points to null and null.

;; Ex. 3.17

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  
  (set-cdr! (last-pair x) y)
  x)

(define (find el list)
  (cond ((eq? '() list) false)
	((eq? (car list) el) true)
	(else (find el (cdr list)))))

(define (traverse structure)
  (define (traverse-h structure visited)
    (if (not (pair? structure))
	0
	(+ (if (find structure visited)
	       0
	       (begin (append! visited (list structure))		      
		      1))
	   (traverse-h (car structure) visited)
	   (traverse-h (cdr structure) visited))))
  (traverse-h structure (list (cons 1 1))))

;;3.18
(define (has-cycle? structure)
  (define (has-cycle-h? structure visited)
    (if (not (pair? structure))
	false
	(if (find structure visited)
	    true
	    (has-cycle-h? (cdr structure) (append visited (list structure))))))
  (has-cycle-h? structure '((cons 1 1))))


;;3.19
(define (has-cycle-c? structure)
					;https://en.wikipedia.org/wiki/Pairing_function
  (define (fsth z)
    (let* ((w (floor (/ (- (sqrt (+ (* 8 z) 1))
			   1)
			2)))
	   (t (/ (+ (* w w) w) 2)))
      (+ (- w z) t)))
  
  (define (sndh z)
    (let* ((w (floor (/ (- (sqrt (+ (* 8 z) 1))
			   1)
			2)))
	   (t (/ (+ (* w w) w) 2)))
      (- z t)))

  (define (i-th-pair l i)
    (if (= i 0)
	l
	(i-th-pair (cdr l) (- i 1))))
  
  ;;do we have cycle of length 'cycle-length' starting in start
  (define (cycle? start cycle-length)
    (define (cycle-h? start cur cycle-length)
      (cond ((eq? cur '()) false)
	    ((= cycle-length 0) (eq? cur start))
	    (else (cycle-h? start (cdr cur) (- cycle-length 1)))))
    (cycle-h? start start cycle-length))
  
  (define (has-length? start length)
    (cond ((and (eq? start '())
		(= length 0)) true)
	  ((and (not (eq? start '()))
		(not (= length 0)))
	   (has-length? (cdr start) (- length 1)))
	  (else false)))

  ;;do we have a cycle starting in 'start-element-i' with length 'cycle-length'
  (define (has-cycle-c-h? n)
    (let ((cycle-length (fsth n))
	  (idx (sndh n)))
      (cond ((= cycle-length 0) (has-cycle-c-h? (+ n 1)))
	    ((has-length? structure idx) false)
	    ((cycle? (i-th-pair structure idx) cycle-length) true)
	    (else (has-cycle-c-h? (+ n 1))))))
  (has-cycle-c-h? 0))

;;3.20 env created by cons contains x and y. procedure z, that is taken from
;;(cons x y) points to env with x and y. car returns x from that environment
;; cdr - similar, set-car! sets x in this frame equal to v, set-cdr! similar

;;3.21 queue is a pair and is printed as a pair.
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))



;;3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (get-front-ptr) front-ptr)
    (define (get-rear-ptr) rear-ptr)
    (define (insert-queue! el) (let ((new-last (list el)))
				 (if (empty-queue?)
				     (begin (set! front-ptr new-last)
					    (set! rear-ptr new-last))
				     (begin (set-cdr! rear-ptr new-last)
					    (set! rear-ptr new-last)))))
    (define (delete-queue!) (if (empty-queue?)
				(error "Queue is empty")
				(set! front-ptr (cdr front-ptr))))
    (define (empty-queue?) (eq? front-ptr '()))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) get-front-ptr)
	    ((eq? m 'rear-ptr) get-rear-ptr)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    ((eq? m 'empty-queue?) empty-queue?)
	    (else (error "Unknown operation"))))
    dispatch))
(define (front-ptr q) ((q 'front-ptr)))
(define (rear-ptr q) ((q 'rear-ptr)))
(define (insert-queue! q el) ((q 'insert-queue!) el))
(define (delete-queue! q) ((q 'delete-queue!)))
(define (empty-queue? q) ((q 'empty-queue?)))

(define (print-queue queue)
  (display (front-ptr queue)))

(define (test-print-queue)
  (define q1 (make-queue))
  (insert-queue! q1 'a)
  (print-queue q1)
  (display "\n")
  (insert-queue! q1 'b)
  (print-queue q1)
  (display "\n")
  (delete-queue! q1)
  (print-queue q1)
  (display "\n")
  (delete-queue! q1)
  (print-queue q1)
  (display "\n"))

;;3.23
;; insert to front (set! front-ptr (cons elem front-ptr))
;; each node stores pointer to prev node
;; delete-last approx (set! rear-ptr (prev-node rear-ptr)) (set-cdr! rear-ptr '())


;;Part 3.3 code
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;Ex 3.24
(define (make-table same-key?)
  (let ((table (list '*table*)))

    (define (lookup key)
      (let ((record (assoc key (cdr table))))
	(if record
	    (cdr record)
	    false)))
    
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))


    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! table
		      (cons (cons key value) (cdr table)))))
      'ok)

    (define (dispatch operation)
      (cond ((eq? operation 'insert!) insert!)
	    ((eq? operation 'lookup) lookup)
	    (else (error "Unknown procedure"))))
    dispatch))


(define operation-table (make-table equal?))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))

;;Ex 3.25. Artitrary sized tables
;;Table entry is ((key . value) next-table)
(define (make-table same-key?)
  (let ((table (list (cons '*table* '()))))
    (define (lookup keys)
      (define (lookup-helper keys table)
	(cond ((eq? keys '()) (cdar table))
	      (else (let* ((first-key (car keys))
			   (subtable (assoc first-key (cdr table))))
		      (if subtable
			  (lookup-helper (cdr keys) subtable)
			  false)))))
      (lookup-helper keys table))
    
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caaar records)) (car records))
	    (else (assoc key (cdr records)))))


    (define (insert! keys value)
      (define (insert!-helper keys value table)
	(cond ((eq? keys '()) (set-cdr! (car table) value))
	      (else (let* ((first-key (car keys))
			   (subtable (assoc first-key (cdr table))))
		      (if subtable
			  (insert!-helper (cdr keys) value subtable)
			  (begin (set-cdr! table (cons (cons (cons first-key '()) '())
						(cdr table)))
				 (insert!-helper (cdr keys) value (cadr table)))))))
	'ok)
      (insert!-helper keys value table))
      
    (define (dispatch operation)
      (cond ((eq? operation 'insert!) insert!)
	    ((eq? operation 'lookup) lookup)
	    (else (error "Unknown procedure"))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))


;;Ex 3.26
;;TreeMap is standard implementation of map.

;;Ex 3.27
;;In general memo-fib will need O(N^2)
;;But for (fib mod K) is will need O(N) because each fib will be computed once.

make-table

;;Part 3.4 TODO
