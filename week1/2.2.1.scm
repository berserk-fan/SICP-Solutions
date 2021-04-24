define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(define (reverse l)
  (if (null? l)
    l
    (append (reverse (cdr l))
            (list (car l)))))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+ (cc amount
           (except-first-denomination coin-values))
        (cc (- amount
              (first-denomination coin-values))
          coin-values)))))

(define (with-parity i l)
  (cond ((null? l) l)
        ((= i (modulo (car l) 2)) (cons (car l) (with-parity i (cdr l))))
        (else (with-parity i (cdr l)))))

(define (same-parity i . l)
  (with-parity (modulo i 2) (cons i l)))

(define (square-list items)
  (if (null? items)
    items
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
      (map proc (cdr items)))))

(define (square-list items)
  (map (lambda(x) (* x x))
        items))

(define (for-each proc items)
  (if (not (null? items)) (proc (car items)))
  (if (not (null? items)) (for-each proc (cdr items))))

;Exercise 2.25.
;Give combinations of cars and cdrs that will pick 7 from each of the following lists:
;1) (1 3 (5 7) 9)
(define l1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l1)))))

;2) ((7))
(define l2 (list (list 7)))
(car (car l2))

;3) (1 (2 (3 (4 (5 (6 7))))))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7))))))))
(car (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))))

;Exercise 2.26.  Suppose we define x and y to be two lists:
;
;(define x (list 1 2 3))
;(define y (list 4 5 6))
;
;What result is printed by the interpreter in response to evaluating each of the following expressions:
;(append x y)
;(cons x y)
;(list x y)
;(1 2 3 4 5 6)
;((1 2 3) 4 5 6)
;((1 2 3) (4 5 6))

;Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,
;(define x (list (list 1 2) (list 3 4)))
;x
;((1 2) (3 4))
;(reverse x)
;((3 4) (1 2))
;(deep-reverse x)
;((4 3) (2 1))
(define nil (cdr (list)))
(define (deep-reverse l)
  (cond ((null? l) l)
        ((not (pair? l)) l)
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

;Exercise 2.28.  Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,
;
;(define x (list (list 1 2) (list 3 4)))
;
;(fringe x)
;(1 2 3 4)
;
;(fringe (list x x))
;(1 2 3 4 1 2 3 4)
(define (fringe l)
  (cond ((null? l) l)
        ((null? (cdr l)) (if (not (pair? (car l)))
                           l
                           (fringe (car l))))
        (else (append (fringe (list (car l))) (fringe (cdr l))))
  ))

					;Exercise 2.29.  A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

;a.  Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((number? (branch-structure mobile)) (branch-structure mobile))
	(else (+ (total-weight (left-branch mobile))
		 (total-weight (right-branch mobile))))))
					;c.  A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.
(define (torque branch)
  (* (total-weight branch) (branch-length branch)))

(define (mobile-balanced? mobile)
  (cond ((number? mobile) (= 1 1))
	(else (and (= (torque (right-branch mobile)) (torque (left-branch mobile)))
		   (mobile-balanced? (branch-structure (right-branch mobile)))
		   (mobile-balanced? (branch-structure (left-branch mobile)))))))

;; d.  Suppose we change the representation of mobiles so that the constructors are
;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))
;; How much do you need to change your programs to convert to the new representation?

;; Answer: We need to change nothing

;; Exercise 2.30.  Define a procedure square-tree analogous to the square-list procedure of exercise 2.21. That is, square-list should behave as follows:

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

;; Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.

(define (square-tree tree)
  (tree-map (lambda(x) (* x x)) tree))

;; Exercise 2.31.  Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

(define (square-tree tree) (tree-map square tree))

(define (tree-map func tree)
  (map (lambda(sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (func sub-tree)))
       tree))

;; Exercise 2.32.  We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:
;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map <??> rest)))))
(define nil (list))
(define (make-set . elements)
  elements)
(define (union set1 set2)
  (append set1 set2))
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda(set) (union (make-set (car s)) set)) rest)))))

;; Exercise 2.33.  Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:
;; (define (map p sequence)
;;   (accumulate (lambda (x y) <??>) nil sequence))
;; (define (append seq1 seq2)
;;   (accumulate cons <??> <??>))
;; (define (length sequence)
;;   (accumulate <??> 0 sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;Exercise 2.34. Evaluate polynomial at point using Horner rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:
(define (count-leaves tree)
  (accumulate (lambda (sub-tree sub-result)
		(if (pair? sub-tree)
		    (+ (count-leaves tree) sub-result)
		    (+ 1 sub-result)))
	      0
	      tree))

;; Exercise 2.36.  The procedure accumulate-n is similar to accumulate except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results. For instance, if s is a sequence containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence (22 26 30). Fill in the missing expressions in the following definition of accumulate-n:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37. Errata
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; Exercise 2.38

;;Answer: Assosiativity

;;Exercise 2.39.   Complete the following definitions of reverse (exercise 2.18) in terms of fold-right and fold-left from exercise 2.38:

;; (define (reverse sequence)
;; (fold-right (lambda (x y) <??>) nil sequence))
;; (define (reverse sequence)
;; (fold-left (lambda (x y) <??>) nil sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (reverse sequence)
  (fold-right (lambda (element result) (append result (list element))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (result element) (cons element result)) nil sequence))

;; Exercise 2.40.  Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j) with 1< j< i< n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.

(define (flatmap op sequence)
  (fold-right append nil (map op sequence)))

(define (unique-pairs-from-to k n)
  (if (>= k n)
      nil
      (append (map (lambda (x) (list k x))
		   (enumerate-interval (+ k 1) n))
	      (unique-pairs-from-to (+ k 1) n))))

(define (unique-pairs n)
  (unique-pairs-from-to 1 n))

(define (prime? x)
  (= 0
     (length (filter (lambda(k) (= 0
				 (modulo x k)))
		     (enumerate-interval 2 (- x 1))))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;Exercise 2.41.  Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s.

(define (unique-triples n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
						     (enumerate-interval (+ j 1) n)))
				(enumerate-interval (+ i 1) n)))
	   (enumerate-interval 1 n)))

(define (triples-with-sum s n)
  (filter (lambda(triple) (= s
			     (+ (car triple)
				  (car (cdr triple))
				  (car (cdr (cdr triple))))))
	  (unique-triples n)))

(define empty-board (list))

(define (adjoin-position row col positions)
(cons (cons row col) positions))

(define (safe? k positions)
(define row (car (car (filter (lambda(pos) (= k
				     (cdr pos)))
			      positions))))
(= 1
   (length (filter (lambda (pos) (or (= (car pos)
					row)
				     (= (abs (- (car pos) row))
					(abs (- (cdr pos) k)))))
		   positions))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
(queen-cols board-size))


;;Exercise 2.44.  Define the procedure up-split used by corner-split. It is similar to right-split, except that it switches the roles of below and beside.
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;Exercise 2.45.  Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating

(define (split splitted_arrangement arrangement)
(lambda (painter n)
  (if (= n 0)
      painter
      (let (smaller ((split splitted_arrangement arrangment) painter (- n 1)))
	(arrangement painter (splitter_arrangement smaller smaller))))))

;;Exercise 2.46. Vectors abstraction
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect coef v1)
  (make-vect (* coef (xcor-vect v1)) (* coef (ycor-vect v2))))

;;Exercise 2.47.  Here are two possible constructors for frames:
;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))
;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))
;; For each constructor supply the appropriate selectors to produce an implementation for frames.

;;1
(define (edge1-frame f)
  (caddr f))

(define (edge2-frame f)
  (cadr f))

(define (origin-frame f)
  (car f))

;;2
(define (edge1-frame f)
  (cdr (cdr f)))

(define (edge2-frame f)
  (cadr f))

(define (origin-frame f)
  (car f))

;;Exercise 2.48.  A directed line segment in the plane can be represented as a pair of vectors -- the vector running from the origin to the start-point of the segment, and the vector running from the origin to the end-point of the segment. Use your vector representation from exercise 2.46 to define a representation for segments with a constructor make-segment and selectors start-segment and end-segment.

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;;Exercise 2.49.  Use segments->painter to define the following primitive painters:
;; a.  The painter that draws the outline of the designated frame.
;; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
;; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;; d.  The wave painter.

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (outline frame)
  (define origin (origin-frame frame))
  (define edge1 (edge1-frame frame))
  (define edge2 (edge2-frame frame))
  (define edgesum (add-vect edge1 edge2))
  (connect origin edge1 egde2 edgesum))

(define (diagonal-segments frame)
  (define origin (origin-frame frame))
  (define edge1 (edge1-frame frame))
  (define edge2 (edge2-frame frame))
  (define edgesum (add-vect edge1 edge2))
  (list (make-segment origin edgesum) (make-segment edge1 edge2)))

(define (diamond-segments frame)
  (define origin (origin-frame frame))
  (define edge1 (edge1-frame frame))
  (define edge2 (edge2-frame frame))
  (define edgesum (add-vect edge1 edge2))
  (define edge1-center (scale-vect 0.5 (edge1-frame frame)))
  (define edge2-center (scale-vect 0.5 (edge2-frame frame)))
  (define edge1-opposite-center (scale-vect 0.5 (sub-vect egdesum egde1)))
  (define edge2-opposite-center (scale-vect 0.5 (sub-vect egdesum egde2)))
  (connect edge1-center edge2-center edge1-opposite-center edge2-opposite-center))

(define (connect . vectors)
  (define shifted (append (cdr vectors) (list (car vectors))))
  (accumulate-n make-segment nil (list vectors shifted)))

(define (wave-helper)
  (define (wave-head)
    (define v1 (make-vect 0.4 1))
    (define v2 (make-vect 0.6 1))
    (define v3 (make-vect 0.6 0.8))
    (define v4 (make-vect 0.4 0.8))
    (connect v1 v2 v3 v4))
  (define (wave-body)
    (define v1 (make-vect 0.5 0.8))
    (define v2 (make-vect 0.5 0.4))
    (connect v1 v2))
  (define (wave-left-hand)
    (define v1 (make-vect 0.5 0.6))
    (define v2 (make-vect 0.2 0.5))
    (connect v1 v2))
  (define (wave-right-hand)
    (define v1 (make-vect 0.5 0.6))
    (define v2 (make-vect 0.8 0.4))
    (connect v1 v2))
  (define (wave-left-leg)
    (define v1 (make-vect 0.5 0.4))
    (define v2 (make-vect 0.3 0))
    (connect v1 v2))
  (define (wave-right-leg)
    (define v1 (make-vect 0.5 0.4))
    (define v2 (make-vect 0.7 0))
    (connect v1 v2))
  (accumulate append nil (list (wave-head)
			       (wave-body)
			       (wave-left-hand)
			       (wave-right-hand)
			       (wave-left-leg)
			       (wave-right-leg)))))
(define wave (wave-helper))


