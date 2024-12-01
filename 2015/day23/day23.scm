
(require rackunit)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 23: Opening the Turing Lock ---
;
; Little Jane Marie just got her very first computer for Christmas from some
;  unknown benefactor. It comes with instructions and an example program, but
;  the computer itself seems to be malfunctioning. She's curious what the
;  program does, and would like you to help her run it.
;
; The manual explains that the computer supports two registers and six instructions
;  (truly, it goes on to remind the reader, a state-of-the-art technology). The
;  registers are named a and b, can hold any non-negative integer, and begin with a
;  value of 0. The instructions are as follows:
;
; * hlf r sets register r to half its current value, then continues with the next instruction.
; * tpl r sets register r to triple its current value, then continues with the next instruction.
; * inc r increments register r, adding 1 to it, then continues with the next instruction.
; * jmp offset is a jump; it continues with the instruction offset away relative to itself.
; * jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
; * jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).
;
; All three jump instructions work with an offset relative to that instruction. The
;  offset is always written with a prefix + or - to indicate the direction of the jump
;  (forward or backward, respectively). For example, jmp +1 would simply continue with
;  the next instruction, while jmp +0 would continuously jump back to itself forever.
;
; The program exits when it tries to run an instruction beyond the ones defined.
;
; For example, this program sets a to 2, because the jio instruction causes it to skip the tpl instruction:
;
; inc a
; jio a, +2
; tpl a
; inc a
;
; What is the value in register b when the program in your puzzle input is finished executing? (Answer: 170)
;

(define (extract line) ; extract : str -> (cmd reg offset)
  (cdr (car (regexp-match* #rx"^([^ ]+) ([ab])?,? ?([+-][0-9]+)?" line #:match-select values))))

(define (get-commands filename)
  (list->vector (map extract (file->lines filename))))

(define (mod-reg command reg)
    (case command
      (("hlf") (/ reg 2))
      (("tpl") (* 3 reg))
      (("inc") (add1 reg))
      (else reg)))

(define (get-reg reg-str reg-a reg-b)
  (if (equal? reg-str "a") reg-a reg-b))

(define (do-offset str idx)
  (+ idx (string->number str)))

; What a kludge! :-/
(define (foobar command reg-str reg-verify reg)
  (if (equal? reg-str reg-verify)
      (mod-reg command reg)
      reg))

(define (exec commands idx reg-a reg-b)
  (if (>= idx (vector-length commands))
      (cons reg-a reg-b)
      (let* ((info (vector-ref commands idx))
             (command  (car info))
             (register (cadr info))
             (offset   (caddr info)))
        (case command
          (("hlf") (exec commands (add1 idx) (foobar command "a" register reg-a) (foobar command "b" register reg-b)))
          (("tpl") (exec commands (add1 idx) (foobar command "a" register reg-a) (foobar command "b" register reg-b)))
          (("inc") (exec commands (add1 idx) (foobar command "a" register reg-a) (foobar command "b" register reg-b)))
          (("jmp") (exec commands (do-offset offset idx) reg-a reg-b))
          (("jie")
           (if (even? (get-reg register reg-a reg-b))
               (exec commands (do-offset offset idx) reg-a reg-b)
               (exec commands (add1 idx) reg-a reg-b)))
          (("jio")
           (if (= 1 (get-reg register reg-a reg-b))
               (exec commands (do-offset offset idx) reg-a reg-b)
               (exec commands (add1 idx) reg-a reg-b)))
          (else
            (error (printf "Unknown command '~a' at index: ~a.~n" command idx)))))))

(check-equal? (car (exec (get-commands "test-data.txt") 0 0 0)) 2) ; Test - Register 'a' should be 2

(check-equal? (cdr (exec (get-commands "day23-data.txt") 0 0 0)) 170) ; Part One - Register 'b' is 170

; --- Part Two ---
;
; The unknown benefactor is very thankful for releasi-- er, helping little Jane Marie with her computer.
;  Definitely not to distract you, what is the value in register b after the program is finished executing
;  if register a starts as 1 instead? (Answer: 247)
;
(check-equal? (cdr (exec (get-commands "day23-data.txt") 0 1 0)) 247) ; Part Two - Register 'b' is 247

