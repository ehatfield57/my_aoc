
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 11: Hex Ed ---
;
; Crossing the bridge, you've barely reached the other side of the
;  stream when a program comes up to you, clearly in distress. "It's
;  my child process," she says, "he's gotten lost in an infinite grid!"
;
; Fortunately for her, you have plenty of experience with infinite grids.
;
; Unfortunately for you, it's a hex grid.
;
; The hexagons ("hexes") in this grid are aligned such that adjacent hexes
;  can be found to the north, northeast, southeast, south, southwest, and
;  northwest:
;
;   \ n  /
; nw +--+ ne
;   /    \
; -+      +-
;   \    /
; sw +--+ se
;   / s  \
;
; You have the path the child process took. Starting where he started, you
;  need to determine the fewest number of steps required to reach him. (A
;  "step" means to move from the hex you are in to any adjacent hex.)
;
; For example:
;
; * ne,ne,ne is 3 steps away.
; * ne,ne,sw,sw is 0 steps away (back where you started).
; * ne,ne,s,s is 2 steps away (se,se).
; * se,sw,se,sw,sw is 3 steps away (s,s,sw).
;
; (Answer: 698)
;

(define (steps-away dir-str)
  (let ((dir-lst (string-split dir-str ","))
        (origin '(0 0 0)))
    (for ((dir dir-lst))
         (define-values (q r s) (apply values origin))
         ; Hex directional magic from: https://www.redblobgames.com/grids/hexagons/#neighbors
         (case dir
           (("ne") (set! q (add1 q)) (set! r (sub1 r)))
           (("se") (set! q (add1 q)) (set! s (sub1 s)))
           (("s")  (set! r (add1 r)) (set! s (sub1 s)))
           (("sw") (set! r (add1 r)) (set! q (sub1 q)))
           (("nw") (set! s (add1 s)) (set! q (sub1 q)))
           (("n")  (set! s (add1 s)) (set! r (sub1 r))))
         (set! origin (list q r s))
         (printf "Child currently ~a steps away.~n" (apply max (map abs origin))))
    (apply max (map abs origin))))

(check-equal? (steps-away "ne,ne,ne") 3)
(check-equal? (steps-away "ne,ne,sw,sw") 0)
(check-equal? (steps-away "ne,ne,s,s") 2)
(check-equal? (steps-away "se,sw,se,sw,sw") 3)

(define (part-one)
  (steps-away (car (file->lines "day11-data.txt"))))

(check-equal? (part-one) 698)

;
; --- Part Two ---
;
; How many steps away is the furthest he ever got from his starting position?
; (Answer: 1435)
;

; Added a print statement and looked at output: 1435 steps

