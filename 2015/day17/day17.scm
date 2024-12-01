
(require rackunit)
(require racket/trace)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 17: No Such Thing as Too Much ---
;
; The elves bought too much eggnog again - 150 liters this time. To fit it
;  all into your refrigerator, you'll need to move it into smaller containers.
;  You take an inventory of the capacities of the available containers.
;
; For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
;  If you need to store 25 liters, there are four ways to do it:
;
; 15 and 10
; 20 and 5 (the first 5)
; 20 and 5 (the second 5)
; 15, 5, and 5
;
; Filling all containers entirely, how many different combinations of
;  containers can exactly fit all 150 liters of eggnog? (Answer: 1304)
;

(define test-data '(25 . (20 15 10 5 5)))
(define part-one-data '(150 . (33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42)))

(define (routine1 goal lst stk)
  (let ((stk-sum (apply + stk)))
   (cond
     ((= goal stk-sum) (cons stk (routine1 goal lst (cdr stk))))
     ((null? lst) '())
     ((> goal stk-sum) (routine1 goal (cdr lst) (cons (car lst) stk)))
     ((< goal stk-sum) (routine1 goal lst (cdr stk))))))

;(trace routine1)

(check-equal? (routine1 25 '(20 15 10 5 5) '()) '((5 20) (5 20)))
(check-equal? (routine1 25 '(15 10 5 5) '()) '((10 15) (5 5 15)))

(define (routine2 goal lst)
  (cond
    ((null? lst) '())
    ((> goal (apply + lst)) '())
    (else 
      (append (routine1 goal lst '())
              (routine2 goal (cdr lst))))))

(define (part-one suite) (routine2 (car suite) (sort (cdr suite) >)))

(check-equal? (part-one test-data) '((5 20) (5 20) (10 15) (5 5 15)))

(check-equal? (length (part-one part-one-data)) 7) ; '7' is the wrong answer!!

;
; Sadly, this gave the correct answer:
;

(define combos (combinations '(33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42)))
(define solutions (for*/list ((lst combos) #:when (= 150 (apply + lst))) lst)) ; -> 1304 lines
(check-equal? (length solutions) 1304)


;
; --- Part Two ---
;
; While playing with all the containers in the kitchen, another load of eggnog arrives!
;  The shipping and receiving department is requesting as many containers as you can spare.
;
; Find the minimum number of containers that can exactly fit all 150 liters of eggnog.
;  How many different ways can you fill that number of containers and still hold exactly
;  150 litres?
;
; In the example above, the minimum number of containers was two. There were three ways
;  to use that many containers, and so the answer there would be 3.  (Answer: 18)
;

(define (get-combos)
  (let ((amts (make-hash)))
   (call-with-input-file
     "part-one-solutions.txt"
     (lambda (in)
       (do ((line (read-line in) (read-line in)))
           ((eof-object? line))
           (let ((len (length (read (open-input-string line)))))
            (when (not (hash-has-key? amts len)) (hash-set! amts len '()))
            (hash-set! amts len (cons line (hash-ref amts len)))))))
   amts))

(define amounts (get-combos))
(check-equal? (length (hash-ref amounts 4)) 18)

