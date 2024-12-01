
(require rackunit)
; (require racket/trace)

;
; Avent of Code 2015 - Part One
;
; --- Day 10: Elves Look, Elves Say ---
;
; Today, the Elves are playing a game called look-and-say. They take turns making
;  sequences by reading aloud the previous sequence and using that reading as the
;  next sequence. For example, 211 is read as "one two, two ones", which becomes
;  1221 (1 2, 2 1s).
;
; Look-and-say sequences are generated iteratively, using the previous value as
;  input for the next step. For each step, take the previous value, and replace each
;  run of digits (like 111) with the number of digits (3) followed by the digit itself (1).
;
; For example:
;
; * 1 becomes 11 (1 copy of digit 1).
; * 11 becomes 21 (2 copies of digit 1).
; * 21 becomes 1211 (one 2 followed by one 1).
; * 1211 becomes 111221 (one 1, one 2, and two 1s).
; * 111221 becomes 312211 (three 1s, two 2s, and one 1).
;
; Starting with the digits in your puzzle input, apply this process 40 times.
;  What is the length of the result?
;
; Your puzzle input is '1113222113'  (Answer: 252594)
;

(define (look-and-say str)
  (define (num->char n) (integer->char (+ 48 n)))
  (check-equal? (num->char 1) #\1)

  (define (helper lst ch cnt out)
    (cond
      ((and (null? lst) (zero? cnt)) out)

      ((null? lst)
       (reverse (cons ch (cons (num->char cnt) out))))

      ((char=? (car lst) ch)
       (helper (cdr lst) (car lst) (add1 cnt) out))

      ((> cnt 0)
       (helper (cdr lst) (car lst) 1 (cons ch (cons (num->char cnt) out))))

      (else
        (helper (cdr lst) (car lst) 1 out))))

  (check-equal? (helper '(#\1) #\x 0 '()) '(#\1 #\1))
  (list->string (helper (string->list str) #\a 0 '())))

(check-equal? "11" (look-and-say "1"))
(check-equal? "21" (look-and-say "11"))
(check-equal? "1211" (look-and-say "21"))
(check-equal? "111221" (look-and-say "1211"))

(sequence->list (in-inclusive-range 1 40))

(define part-one "1113222113")
(for ((i (in-inclusive-range 1 40)))
     (set! part-one (look-and-say part-one)))
(check-equal? 252594 (string-length part-one))

;
; --- Part Two ---
; Neat, right? You might also enjoy hearing John Conway talking about this
;  sequence (that's Conway of Conway's Game of Life fame).
;
; Now, starting again with the digits in your puzzle input, apply this
;  process 50 times. What is the length of the new result? (Answer: 3579328)

(define part-two "1113222113")
(for ((i (in-inclusive-range 1 50)))
     (set! part-two (look-and-say part-two)))
(check-equal? 3579328 (string-length part-two))

