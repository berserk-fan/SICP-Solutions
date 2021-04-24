;;Ex 3.38
;; a. Max - 3! = 6 values.
;; 1) +10 -20 /2 = 45
;; 2) +10 /2 -20 = 35
;; 3) /2 +10 -20 = 40
;; ...
;; b. 110

;;Ex 3.39
;;101:	P1 sets x to 100 and then P2 increments x to 101.
;; 121:	P2 increments x to 11 and then P1 sets x to x times x.
;; 110:	P2 changes x from 10 to 11 between the two times that P1 accesses the value of x during the evaluation of (* x x).
;; 11:	P2 accesses x, then P1 sets x to 100, then P2 sets x.
;; 100:	P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.

;; 101 and 121 will be here
;; 110 impossible because multiplication is not done concurrently with incrementation
;; 100 - possible, 1) caclulate P1 = 100 then set x to 11 then set x to 100
;; 11 - possible: P1 = 100 then during set! x 100 P2 starts and creates 11. Then set! x 100 happens and then set! x 11 happens.

;;Ex 3.40
;;instructions in P1: get x, get x, set x // 3 op
;;instructions in P2; get x, get x, get x, set x // 4 ops
;; number of sequences = 7! / (4! * 3!) = 35 possible executions.
;;10^2 - possible compute p1 then (* x x) then compute p2 (* x x x) then assing x to 1000 then assign x to 100
;;10^3 - also possible, same as prev but set x in p2 is last
;;10^4 - possible, get x get x in p2 then set in p1 then get x in p2 then set in p2. 10 * 10 * 10^2 = 10^4
;;10^5 - same as prev but set x in p1 happened earlier
;;10^6 - possible (serialized case)

;; in serialized there is only 1 value = 10^6.
;;Ex 3.41
;;For this situation it is ok, because operations in account doesn't have intermidiate values of balance. for more complicated procedures it is true.

;;Ex 3.42
;;Ben's approach is an error, because is will be possible to call two withdraw procedures concurrently. (define a1 (acc 'withraw)) (define a2 (acc 'withdraw)) is same procedure, thus it can be called concurrenlty with itself -> errors (define acc (make-account 100)). => (withdraw acc 10), (withdraw acc 10) can result in (= 90 (balance acc)).

;;3.43
;;... skip
