
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 5: Alchemical Reduction ---
;
; You've managed to sneak in to the prototype suit manufacturing lab.
;  The Elves are making decent progress, but are still struggling with
;  the suit's size reduction capabilities.
;
; While the very latest in 1518 alchemical technology might have solved
;  their problem eventually, you can do better. You scan the chemical
;  composition of the suit's material and discover that it is formed
;  by extremely long polymers (one of which is available as your puzzle
;  input).
;
; The polymer is formed by smaller units which, when triggered, react
;  with each other such that two adjacent units of the same type and
;  opposite polarity are destroyed. Units' types are represented by
;  letters; units' polarity is represented by capitalization. For instance,
;  r and R are units with the same type but opposite polarity, whereas r
;  and s are entirely different types and do not react.
;
; For example:
;
; * In aA, a and A react, leaving nothing behind.
; * In abBA, bB destroys itself, leaving aA. As above, this then destroys
;    itself, leaving nothing.
; * In abAB, no two adjacent units are of the same type, and so nothing
;    happens.
; * In aabAAB, even though aa and AA are of the same type, their polarities
;    match, and so nothing happens.
;
; Now, consider a larger example, dabAcCaCBAcCcaDA:
;
; dabAcCaCBAcCcaDA  The first 'cC' is removed.
; dabAaCBAcCcaDA    This creates 'Aa', which is removed.
; dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
; dabCBAcaDA        No further actions can be taken.
;
; After all possible reactions, the resulting polymer contains 10 units.
;
; How many units remain after fully reacting the polymer you scanned?
;  (Note: in this puzzle and others, the input is large; if you copy/paste
;  your input, make sure you get the whole thing.) (Answer: 11636)
;

(define test-data "dabAcCaCBAcCcaDA")

(define (same-char ch-a ch-b)
  (if (or (and (char-upper-case? ch-a) (char-lower-case? ch-b))
          (and (char-upper-case? ch-b) (char-lower-case? ch-a)))
      (if (char-upper-case? ch-a)
          (char=? ch-a (char-upcase ch-b))
          (char=? ch-a (char-downcase ch-b)))
      #f))

(define (part-one str)
  (let ((in (open-input-string str))
        (out (open-output-string)))
    (let loop ((ch (read-char in)))
     (cond
       ((eof-object? ch)
        (if (string=? str (get-output-string out))
            (string-length str)
            (part-one (get-output-string out))))
       ((eof-object? (peek-char in))
        (write-string (string ch) out)
        (loop (read-char in)))
       ((not (same-char ch (peek-char in)))
        (write-string (string ch) out)
        (loop (read-char in)))
       (else
         (read-char in)
         (loop (read-char in)))))))

(check-equal? (part-one test-data) 10)
(check-equal? (part-one (file->string "day05-data.txt")) 11636)

;
; --- Part Two ---
;
; Time to improve the polymer.
;
; One of the unit types is causing problems; it's preventing the polymer
;  from collapsing as much as it should. Your goal is to figure out which
;  unit type is causing the most problems, remove all instances of it
;  (regardless of polarity), fully react the remaining polymer, and measure
;  its length.
;
; For example, again using the polymer dabAcCaCBAcCcaDA from above:
;
; * Removing all A/a units produces dbcCCBcCcD. Fully reacting this polymer
;    produces dbCBcD, which has length 6.
; * Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this polymer
;    produces daCAcaDA, which has length 8.
; * Removing all C/c units produces dabAaBAaDA. Fully reacting this polymer
;    produces daDA, which has length 4.
; * Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this polymer
;    produces abCBAc, which has length 6.
;
; In this example, removing all C/c units was best, producing the answer 4.
;
; What is the length of the shortest polymer you can produce by removing all
;  units of exactly one type and fully reacting the result? (Answer: 5302)
;

(define (part-two str)
  (let ((alphabet (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        (opposite (lambda (a) (integer->char (+ 32 (char->integer a)))))
        (min-string "")
        (min-amount (string-length str))
        (removed-char #\space))
    (for ((unit alphabet))
         (let ((substr (string-replace str (string unit) "")))
          (set! substr (string-replace substr (string (opposite unit)) ""))
          (define size (part-one substr))
          (when (< size min-amount)
            (set! min-amount size)
            (set! min-string substr)
            (set! removed-char unit))))
;    (printf "Removing all '~a' units was the best, producing the answer: ~a~n" removed-char min-amount)
    min-amount))

(check-equal? (part-two test-data) 4)
(check-equal? (part-two (file->string "day05-data.txt")) 5302)

