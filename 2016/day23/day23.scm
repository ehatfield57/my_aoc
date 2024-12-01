
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 23: Safe Cracking ---
;
; This is one of the top floors of the nicest tower in EBHQ. The Easter Bunny's
;  private office is here, complete with a safe hidden behind a painting, and
;  who wouldn't hide a star in a safe behind a painting?
;
; The safe has a digital screen and keypad for code entry. A sticky note attached
;  to the safe has a password hint on it: "eggs". The painting is of a large
;  rabbit coloring some eggs. You see 7.
;
; When you go to type the code, though, nothing appears on the display; instead,
;  the keypad comes apart in your hands, apparently having been smashed. Behind
;  it is some kind of socket - one that matches a connector in your prototype computer!
;  You pull apart the smashed keypad and extract the logic circuit, plug it into your
;  computer, and plug your computer into the safe.
;
; Now, you just need to figure out what output the keypad would have sent to the safe.
;  You extract the assembunny code from the logic chip (your puzzle input).
;
; The code looks like it uses almost the same architecture and instruction set that
;  the monorail computer used! You should be able to use the same assembunny interpreter
;  for this as you did there, but with one new instruction:
;
; tgl x toggles the instruction x away (pointing at instructions like jnz does: positive
;  means forward; negative means backward):
;
; * For one-argument instructions, inc becomes dec, and all other one-argument
;    instructions become inc.
; * For two-argument instructions, jnz becomes cpy, and all other two-instructions
;    become jnz.
; * The arguments of a toggled instruction are not affected.
; * If an attempt is made to toggle an instruction outside the program, nothing happens.
; * If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later
;    made to execute that instruction, skip it instead.
; * If tgl toggles itself (for example, if a is 0, tgl a would target itself and become
;    inc a), the resulting instruction is not executed until the next time it is reached.
;
; For example, given this program:
;
; cpy 2 a
; tgl a
; tgl a
; tgl a
; cpy 1 a
; dec a
; dec a
;
; * cpy 2 a initializes register a to 2.
; * The first tgl a toggles an instruction a (2) away from it, which changes the
;    third tgl a into inc a.
; * The second tgl a also modifies an instruction 2 away from it, which changes
;    the cpy 1 a into jnz 1 a.
; * The fourth line, which is now inc a, increments a to 3.
; * Finally, the fifth line, which is now jnz 1 a, jumps a (3) instructions ahead,
;    skipping the dec a instructions.
;
; In this example, the final value in register a is 3.
;
; The rest of the electronics seem to place the keypad entry (the number of eggs, 7)
;  in register a, run the code, and then send the value left in register a to the safe.
;
; What value should be sent to the safe? (Answer: 11478)
;

(define test-data '("cpy 2 a" "tgl a" "tgl a" "tgl a" "cpy 1 a" "dec a" "dec a"))

(define (machine instructions)
  (let ((registers (make-hash '(("a" . 12) ("b" . 0) ("c" . 0) ("d" . 0))))
        (program (list->vector instructions))
        (p-index 0))

    (define (get-parts instruction)
      (regexp-match* #rx"([^ ]+)" instruction #:match-select car))

    (define (find-number args)
      (if (regexp-match #rx"[abcd]" (car args))
          (hash-ref registers (car args))
          (string->number (car args))))

    (define (replace-letters args)
      (let ((new-args '()))
       (for ((arg args))
            (set! new-args (cons
                             (if (regexp-match #rx"[abcd]" arg)
                                 (hash-ref registers arg)
                                 (string->number arg))
                             new-args)))
       (reverse new-args)))

    (let loop ((instruction (vector-ref program p-index)))
;     (printf "Instruction: '~a', p-index: ~a, registers: ~a~n" instruction p-index registers)
     (let* ((parts (get-parts instruction))
            (cmd (car parts))
            (args (cdr parts)))
       (case cmd
         (("tgl")
          (printf "'tgl', registers: ~a, program: ~a~n" registers program)
          (let ((offset (hash-ref registers (car args))))
           (when (< (+ offset p-index) (vector-length program)) ; Ignore toggles beyond program length
             (let* ((new-inst  (vector-ref program (+ offset p-index)))
                    (new-parts (get-parts new-inst))
                    (new-args  (cdr new-parts))
                    (new-cmd   (car new-parts)))
               (cond
                 ((= 1 (length new-args))
                  (if (string=? new-cmd "inc")
                      (set! new-cmd "dec")
                      (set! new-cmd "inc"))
                  (vector-set! program (+ p-index offset) (string-join (cons new-cmd new-args) " ")))
                 ((= 2 (length new-args))
                  (if (string=? new-cmd "jnz")
                      (set! new-cmd "cpy")
                      (set! new-cmd "jnz"))
                  (vector-set! program (+ p-index offset) (string-join (cons new-cmd new-args) " ")))
                 )))
           (when (not (zero? offset)) (set! p-index (add1 p-index)))) ; If toggled self, execute it next cycle
          )

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
          (if (not (zero? (car (replace-letters args))))
              (set! p-index (+ p-index (cadr (replace-letters args))))
              (set! p-index (add1 p-index))))))

     (if (>= p-index (vector-length program))
         (hash-ref registers "a")
         (loop (vector-ref program p-index))))))

(define (part-one filename)
  (machine (file->lines filename)))

;(check-equal? (machine test-data) 3) ; Register 'a' set to 0
;(check-equal? (part-one "day23-data.txt") 11478) ; Register 'a' set to 7

;
; --- Part Two ---
;
; The safe doesn't open, but it does make several angry noises to express its frustration.
;
; You're quite sure your logic is working correctly, so the only other thing is... you
;  check the painting again. As it turns out, colored eggs are still eggs. Now you count 12.
;
; As you run the program with this new input, the prototype computer begins to overheat.
;  You wonder what's taking so long, and whether the lack of any instruction more powerful
;  than "add one" has anything to do with it. Don't bunnies usually multiply?
;
; Anyway, what value should actually be sent to the safe? (Answer: 479008038)
;

(check-equal? (part-one "day23-data.txt") 479008038) ; Register 'a' set to 12

