
(require rackunit)
(require data/queue)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 18: Duet ---
;
; You discover a tablet containing some strange assembly code labeled simply
;  "Duet". Rather than bother the sound card with it, you decide to run the
;  code yourself. Unfortunately, you don't see any documentation, so you're
;  left to figure out what the instructions mean on your own.
;
; It seems like the assembly is meant to operate on a set of registers that
;  are each named with a single letter and that can each hold a single integer.
;  You suppose each register should start with a value of 0.
;
; There aren't that many instructions, so it shouldn't be hard to figure out
;  what they do. Here's what you determine:
;
; * snd X   plays a sound with a frequency equal to the value of X.
; * set X Y sets register X to the value of Y.
; * add X Y increases register X by the value of Y.
; * mul X Y sets register X to the result of multiplying the value contained
;            in register X by the value of Y.
; * mod X Y sets register X to the remainder of dividing the value contained
;            in register X by the value of Y (that is, it sets X to the result
;            of X modulo Y).
; * rcv X recovers the frequency of the last sound played, but only when the
;          value of X is not zero. (If it is zero, the command does nothing.)
; * jgz X Y jumps with an offset of the value of Y, but only if the value of X
;            is greater than zero. (An offset of 2 skips the next instruction,
;            an offset of -1 jumps to the previous instruction, and so on.)
;
; Many of the instructions can take either a register (a single letter) or a
;  number. The value of a register is the integer it contains; the value of
;  a number is that number.
;
; After each jump instruction, the program continues with the instruction to
;  which the jump jumped. After any other instruction, the program continues
;  with the next instruction. Continuing (or jumping) off either end of the
;  program terminates it.
;
; For example:
;
; set a 1
; add a 2
; mul a a
; mod a 5
; snd a
; set a 0
; rcv a
; jgz a -1
; set a 1
; jgz a -2
;
; * The first four instructions set a to 1, add 2 to it, square it, and then set
;    it to itself modulo 5, resulting in a value of 4.
; * Then, a sound with frequency 4 (the value of a) is played.
; * After that, a is set to 0, causing the subsequent rcv and jgz instructions to
;    both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
; * Finally, a is set to 1, causing the next jgz instruction to activate, jumping
;    back two instructions to another jump, which jumps again to the rcv, which
;    ultimately triggers the recover operation.
;
; At the time the recover operation is executed, the frequency of the last sound
;  played is 4.
;
; What is the value of the recovered frequency (the value of the most recently played
;  sound) the first time a rcv instruction is executed with a non-zero value?
;  (Answer: 1187)
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

(define (player filename)
  (let* ((lines (file->lines filename))
         (program (list->vector lines))
         (prog-len (vector-length program))
         (registers (make-hash))
         (inst-ptr 0)
         (recover  0))
    (let loop ((line (vector-ref program inst-ptr)))
     (let* ((ptr-offset 1)
            (parts (string-split line))
            (cmd (car parts))
            (argx (numberize (cadr parts)))
            (argy (if (> (length parts) 2) (numberize (caddr parts)) #f)))
       (case cmd
         (("set")
          (hash-set! registers argx (get-val registers argy)))

         (("snd")
          (printf "Plays sound with frequency ~a~n" (get-reg registers argx))
          (set! recover (get-reg registers argx)))

         (("add")
          (hash-set! registers argx (+ (get-val registers argy) (get-reg registers argx))))

         (("mul")
          (hash-set! registers argx (* (get-val registers argy) (get-reg registers argx))))

         (("mod")
          (hash-set! registers argx (remainder (get-val registers argx) (get-val registers argy))))

         (("rcv")
          (when (not (zero? (get-val registers argx)))
            (printf "Recovering frequency last played: ~a~n" recover)
            (error (format "The first RCV command has value '~a'~n" recover))))

         (("jgz")
          (when (not (zero? (get-val registers argx)))
            (set! ptr-offset (get-val registers argy))))

         (else (error (format "No such command '~a' available.~n" cmd))))

       (set! inst-ptr (+ inst-ptr ptr-offset))
;       (printf "Hi Edward I, inst-ptr: ~a, ptr-offset: ~a~n" inst-ptr ptr-offset)
       (if (or (< inst-ptr 0) (>= inst-ptr prog-len))
           registers
           (loop (vector-ref program inst-ptr)))))))

;(player "test-data.txt")
;(player "day18-data.txt") ; -> 1187

;
; --- Part Two ---
;
; As you congratulate yourself for a job well done, you notice that the documentation
;  has been on the back of the tablet this entire time. While you actually got most
;  of the instructions correct, there are a few key differences. This assembly code
;  isn't about sound at all - it's meant to be run twice at the same time.
;
; Each running copy of the program has its own set of registers and follows the code
;  independently - in fact, the programs don't even necessarily run at the same speed.
;  To coordinate, they use the send (snd) and receive (rcv) instructions:
;
; * snd X sends the value of X to the other program. These values wait in a queue until
;          that program is ready to receive them. Each program has its own message queue,
;          so a program can never receive a message it sent.
; * rcv X receives the next value and stores it in register X. If no values are in the
;          queue, the program waits for a value to be sent to it. Programs do not continue
;          to the next instruction until they have received a value. Values are received
;          in the order they are sent.
;
; Each program also has its own program ID (one 0 and the other 1); the register p should
;  begin with this value.
;
; For example:
;
; snd 1
; snd 2
; snd p
; rcv a
; rcv b
; rcv c
; rcv d
;
; Both programs begin by sending three values to the other. Program 0 sends 1, 2, 0; program
;  1 sends 1, 2, 1. Then, each program receives a value (both 1) and stores it in a, receives
;  another value (both 2) and stores it in b, and then each receives the program ID of the
;  other program (program 0 receives 1; program 1 receives 0) and stores it in c. Each program
;  now sees a different value in its own copy of register c.
;
; Finally, both programs try to rcv a fourth time, but no data is waiting for either of them,
;  and they reach a deadlock. When this happens, both programs terminate.
;
; It should be noted that it would be equally valid for the programs to run at different speeds;
;  for example, program 0 might have sent all three values and then stopped at the first rcv
;  before program 1 executed even its first instruction.
;
; Once both of your programs have terminated (regardless of what caused them to do so), how many
;  times did program 1 send a value? (Answer: 5969)
;

(define (program prog-num filename my-queue your-queue)
  (let* ((lines (file->lines filename))
         (code (list->vector lines))
         (code-len (vector-length code))
         (p-index 0)
         (registers (make-hash))
         (state 'running)
         (snd-cnt 0)
         (max-snd-cnt 0))
    (hash-set! registers "p" prog-num)
    (lambda (cmd . args)
      (set! snd-cnt 0)
      (case cmd
        ((run)
         (set! state 'running)
         (let loop ((parts (string-split (vector-ref code p-index))))
          (let ((p-offset 1))
           (match parts
                  ((list "snd" x)
                   (set! snd-cnt (add1 snd-cnt))
                   (when (> snd-cnt max-snd-cnt)
                     (printf "Prog: ~a - Max Send Count: ~a~n" prog-num snd-cnt)
                     (set! max-snd-cnt snd-cnt))
                   (enqueue! your-queue (get-val registers x)))
                  ((list "set" x y)
                   (hash-set! registers x (get-val registers y)))
                  ((list "add" x y)
                   (hash-set! registers x (+ (get-val registers y) (get-reg registers x))))
                  ((list "mul" x y)
                   (hash-set! registers x (* (get-val registers y) (get-val registers x))))
                  ((list "mod" x y)
                   (hash-set! registers x (remainder (get-val registers x) (get-val registers y))))
                  ((list "rcv" x)
                   (if (queue-empty? my-queue)
                       (set! state 'pause)
                       (let ((val (dequeue! my-queue)))
                        (hash-set! registers x val))))
                  ((list "jgz" x y)
                   (when (not (zero? (get-val registers x)))
                     (set! p-offset (get-val registers y))))
                  (else (error (format "Program ~a: Not sure what to do with: ~a~n" prog-num parts))))
           (if (eqv? state 'pause)
               (cons state registers)
               (begin
                 (set! p-index (+ p-index p-offset))
                 (when (or (< p-index 0) (>= p-index code-len))
                   (error (format "Program ~a index out of range of code: ~a~n" prog-num p-index)))
                 (loop (string-split (vector-ref code p-index))))))))))))

(define (part-two filename)
  (let* ((queue0 (make-queue))
         (queue1 (make-queue))
         (prog0 (program 0 filename queue0 queue1))
         (prog1 (program 1 filename queue1 queue0))
         (registers0 (prog0 'run))
         (registers1 (prog1 'run)))
    (let loop ((i 0))
     (when (and (queue-empty? queue0) (queue-empty? queue1))
       (error "Programs stalled.~n"))
     (when (not (queue-empty? queue0))
       (set! registers0 (prog0 'run)))
     (when (not (queue-empty? queue1))
       (set! registers1 (prog1 'run)))
     (loop (add1 i)))))

; (part-two "test-data2.txt") ; stalled

; (part-two "day18-data.txt")

; This code never gave the right answer, but I can't imagine how to improve it.
;  Using a reddit solution in python to get past this one.

