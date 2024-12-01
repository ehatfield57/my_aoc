
(require rackunit)

;
; Advent of Code - 2019
;
; Part One
;
; --- Day 4: Secure Container ---
;
; You arrive at the Venus fuel depot only to discover it's protected
;  by a password. The Elves had written the password on a sticky note,
;  but someone threw it out.
;
; However, they do remember a few key facts about the password:
;
; * It is a six-digit number.
; * The value is within the range given in your puzzle input.
; * Two adjacent digits are the same (like 22 in 122345).
; * Going from left to right, the digits never decrease; they only ever
;    increase or stay the same (like 111123 or 135679).
;
; Other than the range rule, the following are true:
;
; * 111111 meets these criteria (double 11, never decreases).
; * 223450 does not meet these criteria (decreasing pair of digits 50).
; * 123789 does not meet these criteria (no double).
;
; How many different passwords within the range given in your puzzle
;  input meet these criteria? (Answer: 1686)
;
; Your puzzle input is 168630-718098.
;

(define *num-size* 6)

(define (num-to-vector num) ; Thanks ChatGPT 4o
  (list->vector (map (lambda (char)
                       (string->number (string char)))
                     (string->list (number->string num)))))

(define (test-size? num)
  (= *num-size* (string-length (number->string num))))

(define (test-dups? num)
  (let* ((num-vec (num-to-vector num))
         (prev-num (vector-ref num-vec 0))
         (ok? #f))
    (for ((num-idx (range 1 *num-size*)))
         (set! ok? (or ok? (= prev-num (vector-ref num-vec num-idx))))
         (set! prev-num (vector-ref num-vec num-idx)))
    ok?))

(define (test-incs? num)
  (let* ((num-vec (num-to-vector num))
         (prev-num (vector-ref num-vec 0))
         (ok? #t))
    (for ((num-idx (range 1 *num-size*)))
         (set! ok? (and ok? (<= prev-num (vector-ref num-vec num-idx))))
         (set! prev-num (vector-ref num-vec num-idx)))
    ok?))

(define (num-ok? num)
  (and (test-size? num)
       (test-dups? num)
       (test-incs? num)))

(check-equal? (num-ok? 122345) #t)
(check-equal? (num-ok? 111123) #t)
(check-equal? (num-ok? 135679) #f)
(check-equal? (num-ok? 111111) #t)
(check-equal? (num-ok? 223450) #f)
(check-equal? (num-ok? 123789) #f)

(define (part-one begin-num end-num)
  (let ((num-cnt 0))
   (for ((the-num (range begin-num (add1 end-num))))
        (when (num-ok? the-num)
          (set! num-cnt (add1 num-cnt))))
   num-cnt))

(define *puzzle-num-1* 168630)
(define *puzzle-num-2* 718098)
(check-equal? (part-one *puzzle-num-1* *puzzle-num-2*) 1686)

;
; --- Part Two ---
;
; An Elf just remembered one more important detail: the two adjacent matching
;  digits are not part of a larger group of matching digits.
;
; Given this additional criterion, but still ignoring the range rule, the
;  following are now true:
;
; * 112233 meets these criteria because the digits never decrease and all
;    repeated digits are exactly two digits long.
; * 123444 no longer meets the criteria (the repeated 44 is part of a
;    larger group of 444).
; * 111122 meets the criteria (even though 1 is repeated more than twice,
;    it still contains a double 22).
;
; How many different passwords within the range given in your puzzle input
;  meet all of the criteria? (Answer: 1145)
;

(define (test-2-dup? num)
  (let* ((num-vec (num-to-vector num))
         (?vec (lambda (n) (vector-ref num-vec n))))
   (cond
     ((and (= (?vec 0) (?vec 1)) (not (= (?vec 1) (?vec 2)))) #t)
     ((and (not (= (?vec 0) (?vec 1))) (= (?vec 1) (?vec 2)) (not (= (?vec 2) (?vec 3)))) #t)
     ((and (not (= (?vec 1) (?vec 2))) (= (?vec 2) (?vec 3)) (not (= (?vec 3) (?vec 4)))) #t)
     ((and (not (= (?vec 2) (?vec 3))) (= (?vec 3) (?vec 4)) (not (= (?vec 4) (?vec 5)))) #t)
     ((and (not (= (?vec 3) (?vec 4))) (= (?vec 4) (?vec 5))) #t)
     (else #f))))

(check-equal? (test-2-dup? 112233) #t)
(check-equal? (test-2-dup? 123444) #f)
(check-equal? (test-2-dup? 111122) #t)

(define (num-ok? num)
  (and (test-size? num)
       (test-incs? num)
       (test-2-dup? num)))

(check-equal? (part-one *puzzle-num-1* *puzzle-num-2*) 1145)

