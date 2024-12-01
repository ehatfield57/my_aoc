
(require rackunit)
(require srfi/1)
(require racket/trace)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 2: Bathroom Security ---
;
; You arrive at Easter Bunny Headquarters under cover of darkness. However, you
;  left in such a rush that you forgot to use the bathroom! Fancy office buildings
;  like this one usually have keypad locks on their bathrooms, so you search the
;  front desk for the code.
;
; "In order to improve security," the document you find says, "bathroom codes will
;  no longer be written down. Instead, please memorize and follow the procedure
;  below to access the bathrooms."
;
; The document goes on to explain that each button to be pressed can be found by
;  starting on the previous button and moving to adjacent buttons on the keypad:
;  U moves up, D moves down, L moves left, and R moves right. Each line of
;  instructions corresponds to one button, starting at the previous button (or,
;  for the first line, the "5" button); press whatever button you're on at the
;  end of each line. If a move doesn't lead to a button, ignore it.
;
; You can't hold it much longer, so you decide to figure out the code as you walk
;  to the bathroom. You picture a keypad like this:
;
; 1 2 3
; 4 5 6
; 7 8 9
;
; Suppose your instructions are:
;
; ULL
; RRDDD
; LURDL
; UUUUD
;
; * You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and
;    stay on "1"), so the first button is 1.
;
; * Starting from the previous button ("1"), you move right twice (to "3") and then
;    down three times (stopping at "9" after two moves and ignoring the third),
;    ending up with 9.
;
; * Continuing from "9", you move left, up, right, down, and left, ending with 8.
;
; * Finally, you move up four times (stopping at "2"), then down once, ending with 5.
;
; So, in this example, the bathroom code is 1985.
;
; Your puzzle input is the instructions from the document you found at the front desk.
;  What is the bathroom code? (Answer: 12578)
;

(define (part-one num dir)
  (case num
    ((1) (case dir ((#\U) 1) ((#\R) 2) ((#\D) 4) ((#\L) 1)))
    ((2) (case dir ((#\U) 2) ((#\R) 3) ((#\D) 5) ((#\L) 1)))
    ((3) (case dir ((#\U) 3) ((#\R) 3) ((#\D) 6) ((#\L) 2)))
    ((4) (case dir ((#\U) 1) ((#\R) 5) ((#\D) 7) ((#\L) 4)))
    ((5) (case dir ((#\U) 2) ((#\R) 6) ((#\D) 8) ((#\L) 4)))
    ((6) (case dir ((#\U) 3) ((#\R) 6) ((#\D) 9) ((#\L) 5)))
    ((7) (case dir ((#\U) 4) ((#\R) 8) ((#\D) 7) ((#\L) 7)))
    ((8) (case dir ((#\U) 5) ((#\R) 9) ((#\D) 8) ((#\L) 7)))
    ((9) (case dir ((#\U) 6) ((#\R) 9) ((#\D) 9) ((#\L) 8)))))

(define (compute instructions (num 5))
  (fold (lambda (inst acc) (part-one acc inst)) num (string->list instructions)))

(define test-data '("ULL" "RRDDD" "LURDL" "UUUUD")) ; -> 1985

(trace compute)
(define (solve data)
  (fold (lambda (str acc) (compute str acc)) 5 data))

(printf "~nPart One, Test Data:~n")
(solve test-data)
(printf "~nPart One, Test Data:~n")
(solve (file->lines "day02-data.txt")) ; -> 12578

;
; --- Part Two ---
;
; You finally arrive at the bathroom (it's a several minute walk from the lobby so
;  visitors can behold the many fancy conference rooms and water coolers on this
;  floor) and go to punch in the code. Much to your bladder's dismay, the keypad
;  is not at all like you imagined it. Instead, you are confronted with the result
;  of hundreds of man-hours of bathroom-keypad-design meetings:
;
;     1
;   2 3 4
; 5 6 7 8 9
;   A B C
;     D
;
; You still start at "5" and stop when you're at an edge, but given the same instructions
;  as above, the outcome is very different:
;
; * You start at "5" and don't move at all (up and left are both edges), ending at 5.
; * Continuing from "5", you move right twice and down three times (through "6", "7", "B",
;    "D", "D"), ending at D.
; * Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
; * Finally, after five more moves, you end at 3.
;
; So, given the actual keypad layout, the code would be 5DB3.
;
; Using the same instructions in your puzzle input, what is the correct bathroom code?
;  (Answer: 516DD)
;

(define (part-two chr dir)
  (case chr
    ((1)   (case dir ((#\U)   1) ((#\R)   1) ((#\D)   3) ((#\L)   1)))
    ((2)   (case dir ((#\U)   2) ((#\R)   3) ((#\D)   6) ((#\L)   2)))
    ((3)   (case dir ((#\U)   1) ((#\R)   4) ((#\D)   7) ((#\L)   2)))
    ((4)   (case dir ((#\U)   4) ((#\R)   4) ((#\D)   8) ((#\L)   3)))
    ((5)   (case dir ((#\U)   5) ((#\R)   6) ((#\D)   5) ((#\L)   5)))
    ((6)   (case dir ((#\U)   2) ((#\R)   7) ((#\D) #\A) ((#\L)   5)))
    ((7)   (case dir ((#\U)   3) ((#\R)   8) ((#\D) #\B) ((#\L)   6)))
    ((8)   (case dir ((#\U)   4) ((#\R)   9) ((#\D) #\C) ((#\L)   7)))
    ((9)   (case dir ((#\U)   9) ((#\R)   9) ((#\D)   9) ((#\L)   8)))
    ((#\A) (case dir ((#\U)   6) ((#\R) #\B) ((#\D) #\A) ((#\L) #\A)))
    ((#\B) (case dir ((#\U)   7) ((#\R) #\C) ((#\D) #\D) ((#\L) #\A)))
    ((#\C) (case dir ((#\U)   8) ((#\R) #\C) ((#\D) #\C) ((#\L) #\B)))
    ((#\D) (case dir ((#\U) #\B) ((#\R) #\D) ((#\D) #\D) ((#\L) #\D)))))

(define (compute instructions (num 5))
  (fold (lambda (inst acc) (part-two acc inst)) num (string->list instructions)))

(define test-data '("ULL" "RRDDD" "LURDL" "UUUUD")) ; -> 5DB3

(trace compute)
(define (solve data)
  (fold (lambda (str acc) (compute str acc)) 5 data))

(printf "~nPart Two, Test Data:~n")
(solve test-data)
(printf "~nPart Two, Real Data:~n")
(solve (file->lines "day02-data.txt")) ; -> 516DD

