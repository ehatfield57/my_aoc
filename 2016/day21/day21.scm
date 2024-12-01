
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 21: Scrambled Letters and Hash ---
;
; The computer system you're breaking into uses a weird scrambling function
;  to store its passwords. It shouldn't be much trouble to create your own
;  scrambled password so you can add it to the system; you just have to
;  implement the scrambler.
;
; The scrambling function is a series of operations (the exact list is provided
;  in your puzzle input). Starting with the password to be scrambled, apply each
;  operation in succession to the string. The individual operations behave as
;  follows:
;
; * swap position X with position Y means that the letters at indexes X and Y
;    (counting from 0) should be swapped.
; * swap letter X with letter Y means that the letters X and Y should be swapped
;    (regardless of where they appear in the string).
; * rotate left/right X steps means that the whole string should be rotated; for
;    example, one right rotation would turn abcd into dabc.
; * rotate based on position of letter X means that the whole string should be
;    rotated to the right based on the index of letter X (counting from 0) as
;    determined before this instruction does any rotations. Once the index is
;    determined, rotate the string to the right one time, plus a number of times
;    equal to that index, plus one additional time if the index was at least 4.
; * reverse positions X through Y means that the span of letters at indexes X
;    through Y (including the letters at X and Y) should be reversed in order.
; * move position X to position Y means that the letter which is at index X
;    should be removed from the string, then inserted such that it ends up at
;    index Y.
;
; For example, suppose you start with abcde and perform the following operations:
;
; * swap position 4 with position 0 swaps the first and last letters, producing
;    the input for the next step, ebcda.
; * swap letter d with letter b swaps the positions of d and b: edcba.
; * reverse positions 0 through 4 causes the entire string to be reversed, producing
;    abcde.
; * rotate left 1 step shifts all letters left one position, causing the first letter
;    to wrap to the end of the string: bcdea.
; * move position 1 to position 4 removes the letter at position 1 (c), then inserts
;    it at position 4 (the end of the string): bdeac.
; * move position 3 to position 0 removes the letter at position 3 (a), then inserts
;    it at position 0 (the front of the string): abdec.
; * rotate based on position of letter b finds the index of letter b (1), then rotates
;    the string right once plus a number of times equal to that index (2): ecabd.
; * rotate based on position of letter d finds the index of letter d (4), then rotates
;    the string right once, plus a number of times equal to that index, plus an additional
;    time because the index was at least 4, for a total of 6 right rotations: decab.
;
; After these steps, the resulting scrambled password is decab.
;
; Now, you just need to generate a new scrambled password and you can access the system.
;  Given the list of scrambling operations in your puzzle input, what is the result of
;  scrambling abcdefgh? (Answer: gbhafcde)
;

(define (extract-values inst)
  (cond
    ((regexp-match #rx"(positions?|letter) . .*?(position|through|letter) ." inst)
     (car (regexp-match* #rx" (.) .* (.)$" inst #:match-select cdr)))

    ((regexp-match #rx"^rotate (left|right|based) (.) step" inst)
     (car (regexp-match* #rx" (.) " inst #:match-select cdr)))

    ((regexp-match #rx"^rotate based on position of letter ." inst)
     (car (regexp-match* #rx" (.)$" inst #:match-select cdr)))

    (else (cons 0 0))))

(define (swap-cmd letters x y)
  (define temp (vector-ref letters x))
  (vector-set! letters x (vector-ref letters y))
  (vector-set! letters y temp))

(define (rotate-left letters)
  (let ((temp (vector-ref letters 0)))
   (for ((i (range (sub1 (vector-length letters)))))
        (vector-set! letters i (vector-ref letters (add1 i))))
   (vector-set! letters (sub1 (vector-length letters)) temp)))

(define (rotate-right letters)
  (let* ((max-idx (sub1 (vector-length letters)))
         (temp (vector-ref letters max-idx)))
    (do ((i max-idx (sub1 i)))
        ((< i 1) #t)
        (vector-set! letters i (vector-ref letters (sub1 i))))
    (vector-set! letters 0 temp)))

(define (rotate-cmd letters x dir)
  (for ((i (range x)))
       (case dir
         ('left  (rotate-left letters))
         ('right (rotate-right letters))
         (else (error "rotate command dir not left or right!")))))

(define (reverse-cmd letters x y)
  (do ((a x (add1 a))
       (b y (sub1 b)))
      ((>= a b) #t)
      (let ((temp (vector-ref letters a)))
       (vector-set! letters a (vector-ref letters b))
       (vector-set! letters b temp))))

(define (move-cmd letters x y)
  (let* ((step (if (< x y) add1 sub1))
         (temp (vector-ref letters x)))
    (do ((i x (step i)))
        ((= i y) (vector-set! letters i temp))
        (vector-set! letters i (vector-ref letters (step i))))))

(define (scramble instructions)
  (lambda (word)
    (let ((letters (list->vector (string->list word))))
     (for ((inst instructions))
          (cond
            ((regexp-match #rx"^swap position . with position ." inst)
             (define-values (x y) (apply values (map string->number (extract-values inst))))
             (swap-cmd letters x y))

            ((regexp-match #rx"^swap letter . with letter ." inst)
             (define-values (x y) (apply values (extract-values inst)))
             (define x-idx (index-of (vector->list letters) (string-ref x 0)))
             (define y-idx (index-of (vector->list letters) (string-ref y 0)))
             (swap-cmd letters x-idx y-idx))

            ((regexp-match #rx"^rotate left . step" inst)
             (define-values (x) (apply values (extract-values inst)))
             (rotate-cmd letters (string->number x) 'left))

            ((regexp-match #rx"^rotate right . step" inst)
             (define-values (x) (apply values (extract-values inst)))
             (rotate-cmd letters (string->number x) 'right))

            ((regexp-match #rx"^rotate based on position of letter ." inst)
             (define-values (x) (apply values (extract-values inst)))
             (define pos (add1 (index-of (vector->list letters) (string-ref x 0))))
             (when (>= pos 5) (set! pos (add1 pos))) ; Because I've added 1 to it already
             (rotate-cmd letters pos 'right))

            ((regexp-match #rx"^reverse positions . through ." inst)
             (define-values (x y) (apply values (extract-values inst)))
             (reverse-cmd letters (string->number x) (string->number y)))

            ((regexp-match #rx"^move position . to position ." inst)
             (define-values (x y) (apply values (extract-values inst)))
             (move-cmd letters (string->number x) (string->number y)))

            (else
              (printf "Unknown instruction: '~a'~n" inst))))
     (list->string (vector->list letters)))))

(define (part-one filename word)
  (let ((scrambler (scramble (file->lines filename))))
   (scrambler word)))

(check-equal? (part-one "test-data.txt" "abcde") "decab")
(check-equal? (part-one "day21-data.txt" "abcdefgh") "gbhafcde")

;
; --- Part Two ---
;
; You scrambled the password correctly, but you discover that you can't actually
;  modify the password file on the system. You'll need to un-scramble one of the
;  existing passwords by reversing the scrambling process.
;
; What is the un-scrambled version of the scrambled password fbgdceah? (Answer: bcfaegdh)
;

(define (unscramble instructions)
  (lambda (word)
    (let ((letters (list->vector (string->list word))))
     (for ((inst instructions))
          (printf "Hi Edward A, letters: ~a, inst: '~a'~n" letters inst)
          (cond
            ((regexp-match #rx"^swap position . with position ." inst)
             (define-values (x y) (apply values (map string->number (extract-values inst))))
             (swap-cmd letters y x)
             (printf "Hi Edward B, swap-cmd, letters: ~a~n" letters))

            ((regexp-match #rx"^swap letter . with letter ." inst)
             (define-values (x y) (apply values (extract-values inst)))
             (define x-idx (index-of (vector->list letters) (string-ref x 0)))
             (define y-idx (index-of (vector->list letters) (string-ref y 0)))
             (swap-cmd letters y-idx x-idx)
             (printf "Hi Edward C, swap-cmd, letters: ~a~n" letters))

            ((regexp-match #rx"^rotate left . step" inst)
             (define-values (x) (apply values (extract-values inst)))
             (rotate-cmd letters (string->number x) 'right)
             (printf "Hi Edward D, rotate-cmd, letters: ~a~n" letters))

            ((regexp-match #rx"^rotate right . step" inst)
             (define-values (x) (apply values (extract-values inst)))
             (rotate-cmd letters (string->number x) 'left)
             (printf "Hi Edward E, rotate-cmd, letters: ~a~n" letters))

            ((regexp-match #rx"^rotate based on position of letter ." inst)
             (define-values (x) (apply values (extract-values inst)))
             (define foo (index-of (vector->list letters) (string-ref x 0)))
             (define pos (vector-ref (vector 1 1 6 2 7 3 0 4) foo)) ; Lookup built by hand
             (rotate-cmd letters pos 'left)
             (printf "Hi Edward F, rotate based, letters: ~a~n" letters))

            ((regexp-match #rx"^reverse positions . through ." inst)
             (define-values (x y) (apply values (extract-values inst)))
             (reverse-cmd letters (string->number x) (string->number y))
             (printf "Hi Edward G, reverse-cmd, letters: ~a~n" letters))

            ((regexp-match #rx"^move position . to position ." inst)
             (define-values (x y) (apply values (extract-values inst)))
             (move-cmd letters (string->number y) (string->number x))
             (printf "Hi Edward H, move-cmd, letters: ~a~n" letters))

            (else
              (printf "Unknown instruction: '~a'~n" inst))))
     (list->string (vector->list letters)))))

(define (part-two filename word)
  (let ((unscrambler (unscramble (reverse (file->lines filename)))))
   (unscrambler word)))

(check-equal? (part-two "day21-data.txt" "gbhafcde") "abcdefgh")
(check-equal? (part-two "day21-data.txt" "fbgdceah") "bcfaegdh")

