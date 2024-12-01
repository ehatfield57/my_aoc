
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 25: The Halting Problem ---
;
; Following the twisty passageways deeper and deeper into the CPU, you
;  finally reach the core of the computer. Here, in the expansive central
;  chamber, you find a grand apparatus that fills the entire room, suspended
;  nanometers above your head.
;
; You had always imagined CPUs to be noisy, chaotic places, bustling with
;  activity. Instead, the room is quiet, motionless, and dark.
;
; Suddenly, you and the CPU's garbage collector startle each other. "It's
;  not often we get many visitors here!", he says. You inquire about the
;  stopped machinery.
;
; "It stopped milliseconds ago; not sure why. I'm a garbage collector, not
;  a doctor." You ask what the machine is for.
;
; "Programs these days, don't know their origins. That's the Turing machine!
;  It's what makes the whole computer work." You try to explain that Turing
;  machines are merely models of computation, but he cuts you off. "No, see,
;  that's just what they want you to think. Ultimately, inside every CPU,
;  there's a Turing machine driving the whole thing! Too bad this one's broken.
;  We're doomed!"
;
; You ask how you can help. "Well, unfortunately, the only way to get the
;  computer running again would be to create a whole new Turing machine from
;  scratch, but there's no way you can-" He notices the look on your face,
;  gives you a curious glance, shrugs, and goes back to sweeping the floor.
;
; You find the Turing machine blueprints (your puzzle input) on a tablet in
;  a nearby pile of debris. Looking back up at the broken Turing machine above,
;  you can start to identify its parts:
;
; * A tape which contains 0 repeated infinitely to the left and right.
; * A cursor, which can move left or right along the tape and read or write
;    values at its current position.
; * A set of states, each containing rules about what to do based on the
;    current value under the cursor.
;
; Each slot on the tape has two possible values: 0 (the starting value for all
;  slots) and 1. Based on whether the cursor is pointing at a 0 or a 1, the
;  current state says what value to write at the current position of the cursor,
;  whether to move the cursor left or right one slot, and which state to use next.
;
; For example, suppose you found the following blueprint:
;
; Begin in state A.
; Perform a diagnostic checksum after 6 steps.
;
; In state A:
;   If the current value is 0:
;     - Write the value 1.
;     - Move one slot to the right.
;     - Continue with state B.
;   If the current value is 1:
;     - Write the value 0.
;     - Move one slot to the left.
;     - Continue with state B.
;
; In state B:
;   If the current value is 0:
;     - Write the value 1.
;     - Move one slot to the left.
;     - Continue with state A.
;   If the current value is 1:
;     - Write the value 1.
;     - Move one slot to the right.
;     - Continue with state A.
;
; Running it until the number of steps required to take the listed diagnostic
;  checksum would result in the following tape configurations (with the cursor
;  marked in square brackets):
;
; ... 0  0  0 [0] 0  0 ... (before any steps; about to run state A)
; ... 0  0  0  1 [0] 0 ... (after 1 step;     about to run state B)
; ... 0  0  0 [1] 1  0 ... (after 2 steps;    about to run state A)
; ... 0  0 [0] 0  1  0 ... (after 3 steps;    about to run state B)
; ... 0 [0] 1  0  1  0 ... (after 4 steps;    about to run state A)
; ... 0  1 [1] 0  1  0 ... (after 5 steps;    about to run state B)
; ... 0  1  1 [0] 1  0 ... (after 6 steps;    about to run state A)
;
; The CPU can confirm that the Turing machine is working by taking a diagnostic
;  checksum after a specific number of steps (given in the blueprint). Once the
;  specified number of steps have been executed, the Turing machine should pause;
;  once it does, count the number of times 1 appears on the tape. In the above
;  example, the diagnostic checksum is 3.
;
; Recreate the Turing machine and save the computer! What is the diagnostic
;  checksum it produces once it's working again? (Answer: 3554)
;

(define (get-instructions filename)
  (let ((lines (file->lines filename))
        (instructions (make-hash))
        (cur-input-state 'Z)
        (cur-input-value 0)
        (cur-input-commands '()))
    (for ((line lines))
         (cond
           ((regexp-match #rx"^Begin in state" line)
            (hash-set! instructions 'start-state (caar (regexp-match* #rx" in state ([A-Z]+)." line #:match-select cdr))))
           ((regexp-match #rx"^Perform a diagnostic checksum" line)
            (hash-set! instructions 'iterations (string->number (car (regexp-match* #rx"([0-9]+)" line)))))
           ((regexp-match #rx"^In state " line)
            (set! cur-input-state (caar (regexp-match* #rx"In state ([A-Z]+):" line #:match-select cdr))))
           ((regexp-match #rx"^  If the current value is" line)
            (set! cur-input-value (string->number (car (regexp-match* #rx"([0-9]+)" line)))))
           ((regexp-match #rx"^    - " line)
            (cond
              ((regexp-match #rx"- Write the value 1." line)
               (set! cur-input-commands (append cur-input-commands (list "W1"))))
              ((regexp-match #rx"- Write the value 0." line)
               (set! cur-input-commands (append cur-input-commands (list "W0"))))
              ((regexp-match #rx"- Move one slot to the left." line)
               (set! cur-input-commands (append cur-input-commands (list "ML"))))
              ((regexp-match #rx"- Move one slot to the right." line)
               (set! cur-input-commands (append cur-input-commands (list "MR"))))
              ((regexp-match #rx"^$" line)
               (set! cur-input-state 'Z)
               (set! cur-input-commands '()))
              (else
                (define next-state (caar (regexp-match* #rx"- Continue with state ([A-Z]+)." line #:match-select cdr)))
                (set! cur-input-commands (append cur-input-commands (list (format "N~a" next-state))))
                (when (not (hash-has-key? instructions cur-input-state))
                  (hash-set! instructions cur-input-state (make-hash)))
                (hash-set! (hash-ref instructions cur-input-state) cur-input-value cur-input-commands)
                (set! cur-input-commands '()))
              ))))
    instructions))


(define (turing filename)
  (let* ((tape (make-hash))
         (instructions (get-instructions filename))
         (cursor 0)
         (state (hash-ref instructions 'start-state))
         (iterations (hash-ref instructions 'iterations))
         (read-tape (lambda ()
                      (when (not (hash-has-key? tape cursor)) (hash-set! tape cursor 0))
                      (hash-ref tape cursor)))
         (write-tape (lambda (val) (hash-set! tape cursor val)))
         (run-commands (lambda (cmds)
                         (for ((cmd cmds))
                              (cond
                                ((string=? cmd "W0") (write-tape 0))
                                ((string=? cmd "W1") (write-tape 1))
                                ((string=? cmd "ML") (set! cursor (sub1 cursor)))
                                ((string=? cmd "MR") (set! cursor (add1 cursor)))
                                (else (set! state (caar (regexp-match* #rx"^N(.)" cmd #:match-select cdr))))))))
         (get-commands (lambda () (hash-ref (hash-ref instructions state) (read-tape))))
         (print-info (lambda (it) (printf "~a - State:~a, Cursor:~a, Tape: ~a~n" it state cursor tape)))
         (count-tape (lambda () (apply + (foldr (lambda (key acc) (cons (hash-ref tape key) acc)) '() (hash-keys tape)))))
         )
    (lambda (cmd)
      (case cmd
        ((reset)
         (set! tape (make-hash))
         (set! cursor 0)
         (set! state (hash-ref instructions 'start-state)))
        ((step)
         (run-commands (get-commands)))
        ((run)
         (for ((i (range iterations)))
;              (print-info i)
              (run-commands (get-commands)))
         (count-tape))
        ((dump) (print-info 0) tape)
        ((count) (count-tape))))))

(define testing (turing "test-data.txt"))
(check-equal? (testing 'run) 3)

(define part-one (turing "day25-data.txt"))

;
; --- Part Two ---
;
; The Turing machine, and soon the entire computer, springs back to life.
;  A console glows dimly nearby, awaiting your command.
;
; > reboot printer
; Error: That command requires priority 50. You currently have priority 0.
; You must deposit 50 stars to increase your priority to the required level.
; The console flickers for a moment, and then prints another message:
;
; Star accepted.
; You must deposit 49 stars to increase your priority to the required level.
; The garbage collector winks at you, then continues sweeping.
;
; If you like, you can [Reboot the Printer Again].
;
; Both parts of this puzzle are complete! They provide two gold stars: **
;
; At this point, all that is left is for you to admire your Advent calendar.
;

