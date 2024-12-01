
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 16: Chronal Classification ---
;
; As you see the Elves defend their hot chocolate successfully, you go
;  back to falling through time. This is going to become a problem.
;
; If you're ever going to return to your own time, you need to understand
;  how this device on your wrist works. You have a little while before
;  you reach your next destination, and with a bit of trial and error,
;  you manage to pull up a programming manual on the device's tiny screen.
;
; According to the manual, the device has four registers (numbered 0 through
;  3) that can be manipulated by instructions containing one of 16 opcodes.
;  The registers start with the value 0.
;
; Every instruction consists of four values: an opcode, two inputs (named A
;  and B), and an output (named C), in that order. The opcode specifies the
;  behavior of the instruction and how the inputs are interpreted. The output,
;  C, is always treated as a register.
;
; In the opcode descriptions below, if something says "value A", it means to
;  take the number given as A literally. (This is also called an "immediate"
;  value.) If something says "register A", it means to use the number given
;  as A to read from (or write to) the register with that number. So, if the
;  opcode addi adds register A and value B, storing the result in register C,
;  and the instruction addi 0 7 3 is encountered, it would add 7 to the value
;  contained by register 0 and store the sum in register 3, never modifying
;  registers 0, 1, or 2 in the process.
;
; Many opcodes are similar except for how they interpret their arguments. The
;  opcodes fall into seven general categories:
;
; Addition:
;
; * addr (add register) stores into register C the result of adding register A and register B.
; * addi (add immediate) stores into register C the result of adding register A and value B.
;
; Multiplication:
;
; * mulr (multiply register) stores into register C the result of multiplying register A and register B.
; * muli (multiply immediate) stores into register C the result of multiplying register A and value B.
;
; Bitwise AND:
;
; * banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
; * bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
;
; Bitwise OR:
;
; * borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
; * bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
;
; Assignment:
;
; * setr (set register) copies the contents of register A into register C. (Input B is ignored.)
; * seti (set immediate) stores value A into register C. (Input B is ignored.)
;
; Greater-than testing:
;
; * gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B.
;    Otherwise, register C is set to 0.
; * gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B.
;    Otherwise, register C is set to 0.
; * gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B.
;    Otherwise, register C is set to 0.
;
; Equality testing:
;
; * eqir (equal immediate/register) sets register C to 1 if value A is equal to register B.
;    Otherwise, register C is set to 0.
; * eqri (equal register/immediate) sets register C to 1 if register A is equal to value B.
;    Otherwise, register C is set to 0.
; * eqrr (equal register/register) sets register C to 1 if register A is equal to register B.
;    Otherwise, register C is set to 0.
;
; Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate
;  the number. However, you can monitor the CPU to see the contents of the registers before
;  and after instructions are executed to try to work them out. Each opcode has a number from
;  0 through 15, but the manual doesn't say which is which. For example, suppose you capture
;  the following sample:
;
; Before: [3, 2, 1, 1]
; 9 2 1 2
; After:  [3, 2, 2, 1]
;
; This sample shows the effect of the instruction 9 2 1 2 on the registers. Before the
;  instruction is executed, register 0 has value 3, register 1 has value 2, and registers
;  2 and 3 have value 1. After the instruction is executed, register 2's value becomes 2.
;
; The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2, B=1, and C=2.
;  Opcode 9 could be any of the 16 opcodes listed above, but only three of them behave in
;  a way that would cause the result shown in the sample:
;
; * Opcode 9 could be mulr: register 2 (which has a value of 1) times register 1 (which has
;    a value of 2) produces 2, which matches the value stored in the output register, register 2.
; * Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1 produces 2,
;    which matches the value stored in the output register, register 2.
; * Opcode 9 could be seti: value 2 matches the value stored in the output register, register 2;
;    the number given for B is irrelevant.
;
; None of the other opcodes produce the result captured in the sample. Because of this, the
;  sample above behaves like three opcodes.
;
; You collect many of these samples (the first section of your puzzle input). The manual also
;  includes a small test program (the second section of your puzzle input) - you can ignore
;  it for now.
;
; Ignoring the opcode numbers, how many samples in your puzzle input behave like three or more
;  opcodes? (Answer: 605)
;

(define (get-registers) (make-vector 4 0))

(define (use-registers registers A B) (list (vector-ref registers A) (vector-ref registers B)))
(define (use-immediate registers A B) (list (vector-ref registers A) B))

(define (set-registersC registers func A B C) (vector-set/copy registers C (apply func (use-registers registers A B))))
(define (set-immediateC registers func A B C) (vector-set/copy registers C (apply func (use-immediate registers A B))))

(define (addr registers A B C) (set-registersC registers + A B C))
(define (addi registers A B C) (set-immediateC registers + A B C))

(define (mulr registers A B C) (set-registersC registers * A B C))
(define (muli registers A B C) (set-immediateC registers * A B C))

(define (banr registers A B C) (set-registersC registers bitwise-and A B C))
(define (bani registers A B C) (set-immediateC registers bitwise-and A B C))

(define (borr registers A B C) (set-registersC registers bitwise-ior A B C))
(define (bori registers A B C) (set-immediateC registers bitwise-ior A B C))

(define (setr registers A B C) (vector-set/copy registers C (vector-ref registers A)))
(define (seti registers A B C) (vector-set/copy registers C A))

(define (gtir registers A B C) (vector-set/copy registers C (if (> A (vector-ref registers B)) 1 0)))
(define (gtri registers A B C) (vector-set/copy registers C (if (> (vector-ref registers A) B) 1 0)))
(define (gtrr registers A B C) (vector-set/copy registers C (if (> (vector-ref registers A) (vector-ref registers B)) 1 0)))

(define (eqir registers A B C) (vector-set/copy registers C (if (= A (vector-ref registers B)) 1 0)))
(define (eqri registers A B C) (vector-set/copy registers C (if (= (vector-ref registers A) B) 1 0)))
(define (eqrr registers A B C) (vector-set/copy registers C (if (= (vector-ref registers A) (vector-ref registers B)) 1 0)))

; Example input data:
;
; Before: [2, 3, 2, 2]
; 0 3 3 0
; After:  [0, 3, 2, 2]
; 
; Before: [1, 1, 2, 3]
; 6 0 2 0
; After:  [0, 1, 2, 3]
; ...
;
; 0 3 3 0
; 6 0 2 0
; ...
;

(define (string->vector str) (apply vector (map string->number (regexp-match* #rx"([0-9]+)" str))))

(define (get-data filename)
  (let ((example '())
        (program '())
        (lines (file->lines filename))
        (before-line "")
        (opABC ""))
    (for ((line lines))
         (cond
           ((regexp-match #rx"^Before:" line) (set! before-line line))
           ((regexp-match #rx"^After:" line)
            (set! example (cons (list (string->vector before-line)
                                      (string->vector opABC)
                                      (string->vector line)) example))
            (set! before-line ""))
           ((regexp-match #rx"[0-9]+ " line)
            (if (string=? before-line "")
                (set! program (cons (string->vector line) program))
                (set! opABC line)))
           ((string=? line "") )
           (else (printf "Unknown value on line:'~a'~n" line))))
    (values (reverse example) (reverse program))))

(define (doit func registers code-vec)
  (func registers (vector-ref code-vec 1) (vector-ref code-vec 2) (vector-ref code-vec 3)))

(define (test-works? func before-vec code-vec after-vec)
  (equal? after-vec (doit func before-vec code-vec)))

(define function-list (list addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr))

(define (test-funcs example)
  (let* ((worked '())
         (before-vec (car example))
         (code-vec (cadr example))
         (after-vec (caddr example))
         (op (vector-ref code-vec 0)))
   (for ((func function-list))
        (when (test-works? func before-vec code-vec after-vec)
          (set! worked (cons func worked))))
   (list op worked)))

(define test-example (list #(3 2 1 1) #(9 2 1 2) #(3 2 2 1)))
(check-equal? (length (cadr (test-funcs test-example))) 3)

(define (part-one filename)
  (let ((sample-cnt 0))
   (define-values (examples program) (get-data filename))
   (for ((example examples))
        (when (>= (length (cadr (test-funcs example))) 3)
          (set! sample-cnt (add1 sample-cnt))))
   sample-cnt))

(check-equal? (part-one "day16-data.txt") 605)

;
; --- Part Two ---
;
; Using the samples you collected, work out the number of each opcode and
;  execute the test program (the second section of your puzzle input).
;
; What value is contained in register 0 after executing the test program?
;  (Answer: 653)
;

(define (opcode-numbers examples)
  (let ((opcodes (make-hash))
        (tally (make-hash))
        (goal-cnt (length examples)))
    ; Fill tally with example info
    (for ((example examples))
         (let* ((results (test-funcs example))
                (op-code (car results))
                (functions (cadr results)))
           (for ((func functions))
                (define f-name (object-name func))
                (when (not (hash-has-key? tally op-code))
                  (hash-set! tally op-code (make-hash)))
                (when (not (hash-has-key? (hash-ref tally op-code) f-name))
                  (hash-set! (hash-ref tally op-code) f-name 0))
                (hash-set! (hash-ref tally op-code) f-name (add1 (hash-ref (hash-ref tally op-code) f-name))))))
    (printf "Hi Edward, tally:~a~n" tally)
    (for ((op-code (hash-keys tally)))
         (printf "Hi Edward B, op-code:~a, length:~a~n" op-code (length (hash-keys (hash-ref tally op-code)))))
    
    ; I figured this out by hand, since I couldn't figure it out in code :-/
    (hash-set! opcodes 0 eqri)
    (hash-set! opcodes 1 mulr)
    (hash-set! opcodes 2 gtri)
    (hash-set! opcodes 3 gtrr)
    (hash-set! opcodes 4 banr)
    (hash-set! opcodes 5 addi)
    (hash-set! opcodes 6 seti)
    (hash-set! opcodes 7 gtir)
    (hash-set! opcodes 8 muli)
    (hash-set! opcodes 9 bori)
    (hash-set! opcodes 10 setr)
    (hash-set! opcodes 11 addr)
    (hash-set! opcodes 12 bani)
    (hash-set! opcodes 13 borr)
    (hash-set! opcodes 14 eqir)
    (hash-set! opcodes 15 eqrr)
    opcodes))

(define (part-two filename)
  (let ((registers #(0 0 0 0)))
   (define-values (examples program) (get-data filename))
   (define opcodes (opcode-numbers examples))
   (for ((code-vec program))
        (set! registers (doit
                          (hash-ref opcodes (vector-ref code-vec 0))
                          registers
                          code-vec)))
   (vector-ref registers 0)))

(check-equal? (part-two "day16-data.txt") 653)

