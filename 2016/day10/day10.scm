
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 10: Balance Bots ---
;
; You come upon a factory in which many robots are zooming around handing
;  small microchips to each other.
;
; Upon closer examination, you notice that each bot only proceeds when it
;  has two microchips, and once it does, it gives each one to a different
;  bot or puts it in a marked "output" bin. Sometimes, bots take microchips
;  from "input" bins, too.
;
; Inspecting one of the microchips, it seems like they each contain a single
;  number; the bots must use some logic to decide what to do with each chip.
;  You access the local control computer and download the bots' instructions
;  (your puzzle input).
;
; Some of the instructions specify that a specific-valued microchip should be
;  given to a specific bot; the rest of the instructions indicate what a given
;  bot should do with its lower-value or higher-value chip.
;
; For example, consider the following instructions:
;
; value 5 goes to bot 2
; bot 2 gives low to bot 1 and high to bot 0
; value 3 goes to bot 1
; bot 1 gives low to output 1 and high to bot 0
; bot 0 gives low to output 2 and high to output 0
; value 2 goes to bot 2
;
; * Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2
;    chip and a value-5 chip.
; * Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its
;    higher one (5) to bot 0.
; * Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives
;    the value-3 chip to bot 0.
; * Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in output 0.
;
; In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a
;  value-2 microchip, and output bin 2 contains a value-3 microchip. In this configuration,
;  bot number 2 is responsible for comparing value-5 microchips with value-2 microchips.
;
; Based on your instructions, what is the number of the bot that is responsible for
;  comparing value-61 microchips with value-17 microchips? (Answer: 113)
;

(struct bot ((nums #:mutable) low high) #:transparent)
; 'nums' is list of chip nums
; 'low' is the location to move low value (ex: "bot 2 gives low to bot 1 ...")
; 'high' is the location to move the high value (ex: "... and high to bot 0)

(define test-instructions '("value 5 goes to bot 2"
                           "bot 2 gives low to bot 1 and high to bot 0"
                           "value 3 goes to bot 1"
                           "bot 1 gives low to output 1 and high to bot 0"
                           "bot 0 gives low to output 2 and high to output 0"
                           "value 2 goes to bot 2"))

(define bots (make-hash))    ; Key: 'numb' of 'bot', Value: struct 'bot'
(define outputs (make-hash)) ; Key: 'output #', Value: List of numbers
(define responsible (make-hash)) ; Key: 'numb', Value: List of (low, high)

(define (update-bot name low-owner high-owner the-val)
  (when (not (hash-has-key? bots name)) (hash-set! bots name (bot '() "" "")))
  (let* ((bot-rec (hash-ref bots name))
         (new-bot (struct-copy bot bot-rec
                               (nums (if (string=? "" the-val) (bot-nums bot-rec) (cons the-val (bot-nums bot-rec))))
                               (low  (if (string=? "" low-owner) (bot-low bot-rec) low-owner))
                               (high (if (string=? "" high-owner) (bot-high bot-rec) high-owner)))))
    (hash-set! bots name new-bot)))

(define (process-value-inst line)
  (let ((parts (car (regexp-match* #rx"^value ([0-9]+) goes to (bot [0-9]+)" line  #:match-select values))))
    (define-values (_ the-val the-bot) (apply values parts))
    (update-bot the-bot "" "" the-val)))

(define (process-bot-inst line)
  (let ((parts (car (regexp-match*
                      #rx"^(bot [0-9]+) gives low to ((bot|output) [0-9]+) and high to ((bot|output) [0-9]+)"
                      line #:match-select values))))
    (define-values (_0 the-bot low-owner _1 high-owner _2) (apply values parts))
    (update-bot the-bot low-owner high-owner "")))

(define (load-instructions lines)
  (for ((line lines))
       (cond
         ((regexp-match #rx"^value " line) (process-value-inst line))
         (else (process-bot-inst line)))))

(define (move-chip-to-bot number the-bot)
  (if (regexp-match #rx"^output " the-bot)
      (begin
        (when (not (hash-has-key? outputs the-bot)) (hash-set! outputs the-bot '()))
        (hash-set! outputs the-bot (cons number (hash-ref outputs the-bot))))
      (let* ((bot-rec (hash-ref bots the-bot))
             (new-bot (struct-copy bot bot-rec (nums (cons number (bot-nums bot-rec))))))
        (hash-set! bots the-bot new-bot))))

(define (run-instructions)
  (let ((the-bots (filter (lambda (bot-name)
                            (>= (length (bot-nums (hash-ref bots bot-name))) 2))
                          (hash-keys bots))))
    (when (not (null? the-bots))
      (define the-bot (car the-bots))
      (when the-bot
        (let* ((bot-rec (hash-ref bots the-bot))
               (nums (sort (map string->number (bot-nums bot-rec)) <))
               (low-chip (number->string (car nums)))
               (high-chip (number->string (cadr nums)))
               (low-bot (bot-low bot-rec))
               (high-bot (bot-high bot-rec)))
          (hash-set! responsible the-bot nums)

          (move-chip-to-bot low-chip low-bot)
          (move-chip-to-bot high-chip high-bot)

          (set-bot-nums! bot-rec '())
          (hash-set! bots the-bot bot-rec)

          (run-instructions))))))

;(load-instructions test-instructions)
(load-instructions (file->lines "day10-data.txt"))
(run-instructions)
responsible

; --- Part Two ---
;
; What do you get if you multiply together the values of one chip in each of
;  outputs 0, 1, and 2? (Answer: 12803)
;
outputs

