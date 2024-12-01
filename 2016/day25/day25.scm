
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 25: Clock Signal ---
;
; You open the door and find yourself on the roof. The city sprawls away from
;  you for miles and miles.
;
; There's not much time now - it's already Christmas, but you're nowhere near
;  the North Pole, much too far to deliver these stars to the sleigh in time.
;
; However, maybe the huge antenna up here can offer a solution. After all, the
;  sleigh doesn't need the stars, exactly; it needs the timing data they provide,
;  and you happen to have a massive signal generator right here.
;
; You connect the stars you have to your prototype computer, connect that to
;  the antenna, and begin the transmission.
;
; Nothing happens.
;
; You call the service number printed on the side of the antenna and quickly explain
;  the situation. "I'm not sure what kind of equipment you have connected over there,"
;  he says, "but you need a clock signal." You try to explain that this is a signal
;  for a clock.
;
; "No, no, a clock signal - timing information so the antenna computer knows how to
;  read the data you're sending it. An endless, alternating pattern of 0, 1, 0, 1, 0,
;  1, 0, 1, 0, 1...." He trails off.
;
; You ask if the antenna can handle a clock signal at the frequency you would need to
;  use for the data from the stars. "There's no way it can! The only antenna we've
;  installed capable of that is on top of a top-secret Easter Bunny installation, and
;  you're definitely not-" You hang up the phone.
;
; You've extracted the antenna's clock signal generation assembunny code (your puzzle
;  input); it looks mostly compatible with code you worked on just recently.
;
; This antenna code, being a signal generator, uses one extra instruction:
;
; * out x transmits x (either an integer or the value of a register) as the next value
;         for the clock signal.
;
; The code takes a value (via register a) that describes the signal to generate, but
;  you're not sure how it's used. You'll have to find the input to produce the right
;  signal through experimentation.
;
; What is the lowest positive integer that can be used to initialize register a and
;  cause the code to output a clock signal of 0, 1, 0, 1... repeating forever? (Answer: 192)
;

(define (machine reg-a-val step-limit instructions)
  (let ((registers (make-hash (list (cons "a" reg-a-val) (cons "b" 0) (cons "c" 0) (cons "d" 0))))
        (program (list->vector instructions))
        (p-index 0)
        (steps 0)
        (op1 (open-output-string)))

    (define (find-number args)
      (if (regexp-match #rx"[abcd]" (car args))
          (hash-ref registers (car args))
          (string->number (car args))))
    
    (let loop ((instruction (vector-ref program p-index)))
     (let* ((parts (regexp-match* #rx"([^ ]+)" instruction #:match-select car))
             (cmd (car parts))
             (args (cdr parts)))
       (case cmd
         (("out")
          (set! steps (add1 steps))
          (write (hash-ref registers (car args)) op1)
          (set! p-index (add1 p-index)))

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

     (if (or (> steps step-limit) (>= p-index (vector-length program)))
         (get-output-string op1)
         (loop (vector-ref program p-index))))))

(define (part-one filename)
  (let loop ((a 0))
    (printf "~a - ~a~n" a (machine a 100 (file->lines filename)))
    (loop (add1 a))))

(part-one "day25-data.txt")

