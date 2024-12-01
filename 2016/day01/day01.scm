
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 1: No Time for a Taxicab ---
;
; Santa's sleigh uses a very high-precision clock to guide its movements, and
;  the clock's oscillator is regulated by stars. Unfortunately, the stars have
;  been stolen... by the Easter Bunny. To save Christmas, Santa needs you to
;  retrieve all fifty stars by December 25th.
;
; Collect stars by solving puzzles. Two puzzles will be made available on each
;  day in the Advent calendar; the second puzzle is unlocked when you complete
;  the first. Each puzzle grants one star. Good luck!
;
; You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
;  unfortunately, is as close as you can get - the instructions on the Easter
;  Bunny Recruiting Document the Elves intercepted start here, and nobody had
;  time to work them out further.
;
; The Document indicates that you should start at the given coordinates (where
;  you just landed) and face North. Then, follow the provided sequence: either
;  turn left (L) or right (R) 90 degrees, then walk forward the given number of
;  blocks, ending at a new intersection.
;
; There's no time to follow such ridiculous instructions on foot, though, so you
;  take a moment and work out the destination. Given that you can only walk on
;  the street grid of the city, how far is the shortest path to the destination?
;
; For example:
;
; * Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
; * R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
; * R5, L5, R5, R3 leaves you 12 blocks away.
;
; How many blocks away is Easter Bunny HQ? (Answer: 230)
;

; Refinement: This IS Scheme, so I should define new syntax for movex/movey - EJH

(define (movex the-hash x y step delta)
  (for ((n (range (+ x step) (+ (* step delta) x step) step)))
       (if (hash-has-key? the-hash (cons n y))
           (hash-set! the-hash (cons n y) (add1 (hash-ref the-hash (cons n y))))
           (hash-set! the-hash (cons n y) 1))
       (when (= 2 (hash-ref the-hash (cons n y)))
         (printf "Second time of hitting point: (~a, ~a)~n" n y))))

(define (movey the-hash x y step delta)
  (for ((n (range (+ y step) (+ (* step delta) y step) step)))
       (if (hash-has-key? the-hash (cons x n))
           (hash-set! the-hash (cons x n) (add1 (hash-ref the-hash (cons x n))))
           (hash-set! the-hash (cons x n) 1))
       (when (= 2 (hash-ref the-hash (cons x n)))
         (printf "Second time of hitting point: (~a, ~a)~n" x n))))

(define (part-one instructions)
  (let (
        (x 0) (y 0) (dir 'N)
        (history (make-hash))
        (steps (regexp-match* #rx"((R|L)([0-9]+))" instructions #:match-select car)))
    (hash-set! history (cons x y) 1) ; Fill history with initial location
    (for ((step steps))
         (let ((turn (string->symbol (substring step 0 1)))
               (dist (string->number (substring step 1))))
           (case turn
             ('L (set! dir (case dir ('N 'W) ('W 'S) ('S 'E) ('E 'N))))
             ('R (set! dir (case dir ('N 'E) ('W 'N) ('S 'W) ('E 'S)))))
           (case dir
             ('N (movey history x y -1 dist) (set! y (- y dist)))
             ('E (movex history x y +1 dist) (set! x (+ x dist)))
             ('S (movey history x y +1 dist) (set! y (+ y dist)))
             ('W (movex history x y -1 dist) (set! x (- x dist))))))
    (+ (abs x) (abs y))))

; Tests:
(check-equal? (part-one "R2, L3") 5)
(check-equal? (part-one "R2, R2, R2") 2)
(check-equal? (part-one "R5, L5, R5, R3") 12)

; Part One Data:
(part-one "R1, L4, L5, L5, R2, R2, L1, L1, R2, L3, R4, R3, R2, L4, L2, R5, L1, R5, L5, L2, L3, L1, R1, R4, R5, L3, R2, L4, L5, R1, R2, L3, R3, L3, L1, L2, R5, R4, R5, L5, R1, L190, L3, L3, R3, R4, R47, L3, R5, R79, R5, R3, R1, L4, L3, L2, R194, L2, R1, L2, L2, R4, L5, L5, R1, R1, L1, L3, L2, R5, L3, L3, R4, R1, R5, L4, R3, R1, L1, L2, R4, R1, L2, R4, R4, L5, R3, L5, L3, R1, R1, L3, L1, L1, L3, L4, L1, L2, R1, L5, L3, R2, L5, L3, R5, R3, L4, L2, R2, R4, R4, L4, R5, L1, L3, R3, R4, R4, L5, R4, R2, L3, R4, R2, R1, R2, L4, L2, R2, L5, L5, L3, R5, L5, L1, R4, L1, R1, L1, R4, L5, L3, R4, R1, L3, R4, R1, L3, L1, R1, R2, L4, L2, R1, L5, L4, L5")

;
;--- Part Two ---
; Then, you notice the instructions continue on the back of the Recruiting Document. Easter
;  Bunny HQ is actually at the first location you visit twice.
;
; For example, if your instructions are R8, R4, R4, R8, the first location you visit twice
;  is 4 blocks away, due East.
;
; How many blocks away is the first location you visit twice? (Answer: 154)
;

(check-equal? (part-one "R8, R4, R4, R8") 8) ; But the important line here was: 'Second time of hitting point: (4, 0)'

