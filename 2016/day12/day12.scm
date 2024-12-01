
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 12: Leonardo's Monorail ---
;
; You finally reach the top floor of this building: a garden with a slanted glass
;  ceiling. Looks like there are no more stars to be had.
;
; While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt
;  some of the files you extracted from the servers downstairs.
;
; According to these documents, Easter Bunny HQ isn't just this building - it's a
;  collection of buildings in the nearby area. They're all connected by a local
;  monorail, and there's another building not far from here! Unfortunately, being
;  night, the monorail is currently not operating.
;
; You remotely connect to the monorail control systems and discover that the boot
;  sequence expects a password. The password-checking logic (your puzzle input) is
;  easy to extract, but the code it uses is strange: it's assembunny code designed
;  for the new computer you just assembled. You'll have to execute the code and get
;  the password.
;
; The assembunny code you've extracted operates on four registers (a, b, c, and d)
;  that start at 0 and can hold any integer. However, it seems to make use of only
;  a few instructions:
;
; * cpy x y  copies x (either an integer or the value of a register) into register y.
; * inc x    increases the value of register x by one.
; * dec x    decreases the value of register x by one.
; * jnz x y  jumps to an instruction y away (positive means forward; negative means
;             backward), but only if x is not zero.
;
; The jnz instruction moves relative to itself: an offset of -1 would continue at the
;  previous instruction, while an offset of 2 would skip over the next instruction.
;
; For example:
;
; cpy 41 a
; inc a
; inc a
; dec a
; jnz a 2
; dec a
;
; The above code would set register a to 41, increase its value by 2, decrease its value
;  by 1, and then skip the last dec a (because a is not zero, so the jnz a 2 skips it),
;  leaving register a at 42. When you move past the last instruction, the program halts.
;
; After executing the assembunny code in your puzzle input, what value is left in
;  register a? (Answer: 318020)
;

(define test-data '("cpy 41 a" "inc a" "inc a" "dec a" "jnz a 2" "dec a"))

(define (machine instructions)
  (let ((registers (make-hash '(("a" . 0) ("b" . 0) ("c" . 0) ("d" . 0))))
        (program (list->vector instructions))
        (p-index 0))

    (define (find-number args)
      (if (regexp-match #rx"[abcd]" (car args))
          (hash-ref registers (car args))
          (string->number (car args))))
    
    ;   (printf "Machine instruction length: ~a~n" (vector-length program))
    (let loop ((instruction (vector-ref program p-index)))
     (let* ((parts (regexp-match* #rx"([^ ]+)" instruction #:match-select car))
             (cmd (car parts))
             (args (cdr parts)))
       (case cmd
         (("cpy")
          (hash-set! registers (cadr args) (find-number args))
          (set! p-index (add1 p-index)))

         (("inc")
          (hash-set! registers (car args) (add1 (hash-ref registers (car args))))
          (set! p-index (add1 p-index)))

         (("dec")
          (hash-set! registers (car args) (sub1 (hash-ref registers (car args))))
          (set! p-index (add1 p-index)))

         (("jnz")
          (if (not (zero? (find-number args)))
              (set! p-index (+ p-index (string->number (cadr args))))
              (set! p-index (add1 p-index))))))

     (if (>= p-index (vector-length program))
         (hash-ref registers "a")
         (loop (vector-ref program p-index))))))

(define (part-one filename)
  (machine (file->lines filename)))

(check-equal? (machine test-data) 42)
(check-equal? (part-one "day12-data.txt") 318020)

;
; --- Part Two ---
;
;  As you head down the fire escape to the monorail, you notice it didn't start;
;   register c needs to be initialized to the position of the ignition key.
;
; If you instead initialize register c to be 1, what value is now left in
;  register a? (Answer: 9227674)

(define (part-two filename)
  (machine (file->lines filename)))

; Modify registers so that 'c' is 1 instead of 0
;(check-equal? (part-two "day12-data.txt") 9227674)

