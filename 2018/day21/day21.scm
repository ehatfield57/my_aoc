
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 21: Chronal Conversion ---
;
; You should have been watching where you were going, because as you wander
;  the new North Pole base, you trip and fall into a very deep hole!
;
; Just kidding. You're falling through time again.
;
; If you keep up your current pace, you should have resolved all of the
;  temporal anomalies by the next time the device activates. Since you have
;  very little interest in browsing history in 500-year increments for the
;  rest of your life, you need to find a way to get back to your present time.
;
; After a little research, you discover two important facts about the
;  behavior of the device:
;
; First, you discover that the device is hard-wired to always send you
;  back in time in 500-year increments. Changing this is probably not feasible.
;
; Second, you discover the activation system (your puzzle input) for the
;  time travel module. Currently, it appears to run forever without halting.
;
; If you can cause the activation system to halt at a specific moment, maybe
;  you can make the device send you so far back in time that you cause an integer
;  underflow in time itself and wrap around back to your current time!
;
; The device executes the program as specified in manual section one and manual
;  section two.
;
; Your goal is to figure out how the program works and cause it to halt. You can
;  only control register 0; every other register begins at 0 as usual.
;
; Because time travel is a dangerous activity, the activation system begins with
;  a few instructions which verify that bitwise AND (via bani) does a numeric
;  operation and not an operation as if the inputs were interpreted as strings.
;  If the test fails, it enters an infinite loop re-running the test instead of
;  allowing the program to execute normally. If the test passes, the program
;  continues, and assumes that all other bitwise operations (banr, bori, and borr)
;  also interpret their inputs as numbers. (Clearly, the Elves who wrote this
;  system were worried that someone might introduce a bug while trying to emulate
;  this system with a scripting language.)
;
; What is the lowest non-negative integer value for register 0 that causes the
;  program to halt after executing the fewest instructions? (Executing the same
;  instruction multiple times counts as multiple instructions executed.)
;  (Answer: 11050031) ; Answer supplied by Reddit
;

(define registers (make-vector 6 0))
(define ip-reg 0)
(define ip 0)

(define (use-registers A B) (list (vector-ref registers A) (vector-ref registers B)))
(define (use-immediate A B) (list (vector-ref registers A) B))

(define (set-registersC func A B C) (vector-set/copy registers C (apply func (use-registers A B))))
(define (set-immediateC func A B C) (vector-set/copy registers C (apply func (use-immediate A B))))

(define (addr A B C) (set-registersC + A B C))
(define (addi A B C) (set-immediateC + A B C))

(define (mulr A B C) (set-registersC * A B C))
(define (muli A B C) (set-immediateC * A B C))

(define (banr A B C) (set-registersC bitwise-and A B C))
(define (bani A B C) (set-immediateC bitwise-and A B C))

(define (borr A B C) (set-registersC bitwise-ior A B C))
(define (bori A B C) (set-immediateC bitwise-ior A B C))

(define (setr A B C) (vector-set/copy registers C (vector-ref registers A)))
(define (seti A B C) (vector-set/copy registers C A))

(define (gtrr A B C) (vector-set/copy registers C (if (> (vector-ref registers A) (vector-ref registers B)) 1 0)))
(define (gtir A B C) (vector-set/copy registers C (if (> A (vector-ref registers B)) 1 0)))

(define (eqrr A B C) (vector-set/copy registers C (if (= (vector-ref registers A) (vector-ref registers B)) 1 0)))
(define (eqri A B C) (vector-set/copy registers C (if (= (vector-ref registers A) B) 1 0)))

(define lookups (hash "addr" addr  "addi" addi  "mulr" mulr  "muli" muli
                      "setr" setr  "seti" seti  "gtrr" gtrr  "eqrr" eqrr
                      "banr" banr  "bani" bani  "borr" borr  "bori" bori
                      "gtir" gtir  "eqri" eqri))

(define (get-program filename)
  (let ((lines (file->lines filename))
        (program '())
        (to-num? (lambda (s) (if (string->number s) (string->number s) s))))
    (for ((line lines))
         (cond
           ((regexp-match #rx"#ip [0-9]" line)
            (set! ip-reg (string->number (car (regexp-match* #rx"([0-9]+)" line)))))
           (else
             (define-values (cmd v1 v2 v3) (apply values (map to-num? (regexp-match* #px"\\S+" line))))
             (set! program (cons (list cmd v1 v2 v3) program)))))
    (list->vector (reverse program))))

(define (execute-cmd program registers (show #f))
  (when show (printf "ip=~a [~a] ~a " ip registers (vector-ref program ip)))
  (vector-set! registers ip-reg ip)
  (define-values (cmd v1 v2 v3) (apply values (vector-ref program ip)))
  (set! registers ((hash-ref lookups cmd) v1 v2 v3))
  (set! ip (add1 (vector-ref registers ip-reg)))
  (when show (printf "[~a]~n" registers)))

(define (part-one filename (show #f))
  (let* ((program (get-program filename))
         (prog-len (vector-length program)))
    (let loop1 ((val 0))
     (set! ip 0)
     (for ((i (range 6))) (vector-set! registers i 0))
     (vector-set! registers 0 val)
     (let loop2 ((cnt 0))
      (when (< cnt 10000)
        (execute-cmd program registers show)
        (if (>= ip prog-len)
            (error "Off the end we go!")
            (loop2 (add1 cnt))))
      (loop1 (add1 val))))))

;
; --- Part Two ---
;
; In order to determine the timing window for your underflow exploit, you also
;  need an upper bound:
;
; What is the lowest non-negative integer value for register 0 that causes the
;  program to halt after executing the most instructions? (The program must
;  actually halt; running forever does not count as halting.) (Answer: 11341721)
;

; Answer supplied by Reddit

