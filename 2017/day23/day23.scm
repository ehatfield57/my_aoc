
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 23: Coprocessor Conflagration ---
;
; You decide to head directly to the CPU and fix the printer from there.
;  As you get close, you find an experimental coprocessor doing so much
;  work that the local programs are afraid it will halt and catch fire.
;  This would cause serious issues for the rest of the computer, so you
;  head in and see what you can do.
;
; The code it's running seems to be a variant of the kind you saw recently
;  on that tablet. The general functionality seems very similar, but some
;  of the instructions are different:
;
; * set X Y sets register X to the value of Y.
; * sub X Y decreases register X by the value of Y.
; * mul X Y sets register X to the result of multiplying the value contained
;    in register X by the value of Y.
; * jnz X Y jumps with an offset of the value of Y, but only if the value of
;    X is not zero. (An offset of 2 skips the next instruction, an offset of
;    -1 jumps to the previous instruction, and so on.)
;
; Only the instructions listed above are used. The eight registers here,
;  named a through h, all start at 0.
;
; The coprocessor is currently set to some kind of debug mode, which allows
;  for testing, but prevents it from doing any meaningful work.
;
; If you run the program (your puzzle input), how many times is the mul
;  instruction invoked? (Answer: 9409)
;

(define (numberize str)
  (if (regexp-match #rx"^[0-9-]+$" str) (string->number str) str))

(define (get-reg registers reg)
  (when (not (hash-has-key? registers reg))
    (hash-set! registers reg 0))
  (hash-ref registers reg))

(define (get-val registers arg)
  (if (number? arg)
      arg
      (if (number? (string->number arg))
          (string->number arg)
          (get-reg registers arg))))

(define mul-cnt 0)

(define (player filename)
  (let* ((lines (file->lines filename))
         (program (list->vector lines))
         (prog-len (vector-length program))
         (registers (make-hash))
         (inst-ptr 0)
         (recover  0))
    (set! mul-cnt 0)
    (let loop ((line (vector-ref program inst-ptr)))
     (let* ((ptr-offset 1)
            (parts (string-split line))
            (cmd (car parts))
            (argx (numberize (cadr parts)))
            (argy (if (> (length parts) 2) (numberize (caddr parts)) #f)))
       (case cmd
         (("set")
          (hash-set! registers argx (get-val registers argy)))

         (("sub")
          (hash-set! registers argx (- (get-val registers argx) (get-reg registers argy))))

         (("mul")
          (set! mul-cnt (add1 mul-cnt))
          (hash-set! registers argx (* (get-val registers argy) (get-reg registers argx))))

         (("jnz")
          (when (not (zero? (get-val registers argx)))
            (set! ptr-offset (get-val registers argy))))

         (else (error (format "No such command '~a' available.~n" cmd))))

       (set! inst-ptr (+ inst-ptr ptr-offset))
       (if (or (< inst-ptr 0) (>= inst-ptr prog-len))
           registers
           (loop (vector-ref program inst-ptr)))))))

; (player "day23-data.txt")
; (printf "Multiply run ~a times.~n" mul-cnt)

; Had to switch to reading/understanding the pseudo assembly code, which I failed at.
; So I used two javascript examples that I found on Reddit to solve this day. :-(

;
; --- Part Two ---
;
; Now, it's time to fix the problem.
;
; The debug mode switch is wired directly to register a. You flip the switch, which
;  makes register a now start at 1 when the program is executed.
;
; Immediately, the coprocessor begins to overheat. Whoever wrote this program obviously
;  didn't choose a very efficient implementation. You'll need to optimize the program
;  if it has any hope of completing before Santa needs that printer working.
;
; The coprocessor's ultimate goal is to determine the final value left in register h
;  once the program completes. Technically, if it had that... it wouldn't even need
;  to run the program.
;
; After setting register a to 1, if the program were to run to completion, what value
;  would be left in register h? (Answer: 913)
;

; Again: Solved with stolen code from reddit. :-(

