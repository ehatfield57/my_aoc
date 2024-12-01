
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 8: I Heard You Like Registers ---
;
; You receive a signal directly from the CPU. Because of your recent
;  assistance with jump instructions, it would like you to compute
;  the result of a series of unusual register instructions.
;
; Each instruction consists of several parts: the register to modify,
;  whether to increase or decrease that register's value, the amount
;  by which to increase or decrease it, and a condition. If the
;  condition fails, skip the instruction without modifying the register.
;  The registers all start at 0. The instructions look like this:
;
; b inc 5 if a > 1
; a inc 1 if b < 5
; c dec -10 if a >= 1
; c inc -20 if c == 10
;
; These instructions would be processed as follows:
;
; * Because a starts at 0, it is not greater than 1, and so b is not modified.
; * a is increased by 1 (to 1) because b is less than 5 (it is 0).
; * c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
; * c is increased by -20 (to -10) because c is equal to 10.
;
; After this process, the largest value in any register is 1.
;
; You might also encounter <= (less than or equal to) or != (not equal to).
;  However, the CPU doesn't have the bandwidth to tell you what all the
;  registers are named, and leaves that to you to determine.
;
; What is the largest value in any register after completing the
;  instructions in your puzzle input? (Answer: 6828)
;

(define (cond-true? reg-num compare chk-num)
  (case compare
    ((">") (> reg-num chk-num))
    (("<") (< reg-num chk-num))
    ((">=") (>= reg-num chk-num))
    (("<=") (<= reg-num chk-num))
    (("==") (= reg-num chk-num))
    (("!=") (not (= reg-num chk-num)))
    (else (error (format "Unknown comparison operator '~a'" compare)))))

(define (largest-register filename)
  (let ((program (file->lines filename))
        (registers (make-hash))
        (highest-reg-num 0))
    (for ((line program))
         (define parts (car (regexp-match* #rx"^([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) (.+?) (-?[0-9]+)" line #:match-select cdr)))
         (define-values (the-reg inc-dec inc-num chk-reg compare chk-num) (apply values parts))
         (when (not (hash-has-key? registers the-reg)) (hash-set! registers the-reg 0))
         (when (not (hash-has-key? registers chk-reg)) (hash-set! registers chk-reg 0))

         (when (cond-true? (hash-ref registers chk-reg) compare (string->number chk-num))
           (let ((func (if (string=? inc-dec "inc") + -)))
            (hash-set! registers the-reg (func (hash-ref registers the-reg) (string->number inc-num)))))

         (let ((high-test-num (apply max (hash-values registers))))
          (when (> high-test-num highest-reg-num)
            (set! highest-reg-num high-test-num)
            (printf "Highest number so far: ~a~n" highest-reg-num))))

    (apply max (hash-values registers))))

(check-equal? (largest-register "test-data.txt") 1)
(check-equal? (largest-register "day08-data.txt") 6828)

;
; --- Part Two ---
;
; To be safe, the CPU also needs to know the highest value held in any register
;  during this process so that it can decide how much memory to allocate to these
;  operations. For example, in the above instructions, the highest value ever held
;  was 10 (in register c after the third instruction was evaluated). (Answer: 7234)
;

