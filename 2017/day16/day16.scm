
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 16: Permutation Promenade ---
;
; You come upon a very unusual sight; a group of programs here appear
;  to be dancing.
;
; There are sixteen programs in total, named a through p. They start
;  by standing in a line: a stands in position 0, b stands in position
;  1, and so on until p, which stands in position 15.
;
; The programs' dance consists of a sequence of dance moves:
;
; * Spin, written sX, makes X programs move from the end to the front,
;    but maintain their order otherwise. (For example, s3 on abcde
;    produces cdeab).
;
; * Exchange, written xA/B, makes the programs at positions A and B swap places.
;
; * Partner, written pA/B, makes the programs named A and B swap places.
;
; For example, with only five programs standing in a line (abcde), they
;  could do the following dance:
;
; * s1, a spin of size 1: eabcd.
; * x3/4, swapping the last two programs: eabdc.
; * pe/b, swapping programs e and b: baedc.
;
; After finishing their dance, the programs end up in order baedc.
;
; You watch the dance for a while and record their dance moves (your puzzle
;  input). In what order are the programs standing after their dance?
;  (Answer: "hmefajngplkidocb")
;

(define test-data '("s1" "x3/4" "pe/b"))

(define (extract ch-list)
  (list (string (car ch-list)) (string (caddr ch-list))))

(define (spin str-list num)
  (define-values (pre post) (split-at-right str-list num))
  (append post pre))

(define (exchange str-list a b)
  (let* ((str-vec (list->vector str-list)))
   (define temp (vector-ref str-vec a))
   (vector-set! str-vec a (vector-ref str-vec b))
   (vector-set! str-vec b temp)
   (vector->list str-vec)))

(define (partner str-list a b)
  (let ((x (index-of str-list (string-ref a 0)))
        (y (index-of str-list (string-ref b 0))))
    (exchange str-list x y)))

(define (get-nums str)
  (map string->number (flatten (regexp-match* #rx"([0-9]+)" str))))

(define (process str prog)
  (let ((the-str (string->list str)))
   (for ((cmd-str prog))
        (let* ((cmd-list (string->list cmd-str))
               (cmd (car cmd-list))
               (ops (cdr cmd-list)))
;          (printf "Hi Edward A, cmd-str: '~a', before: '~a'~n" cmd-str the-str)
          (cond
            ((char=? cmd #\s)
             (set! the-str (spin the-str (car (get-nums cmd-str)))))

            ((char=? cmd #\x)
             (define-values (a b) (apply values (get-nums cmd-str)))
             (set! the-str (exchange the-str a b)))

            ((char=? cmd #\p)
             (let* ((letters (extract ops))
                    (a (car letters))
                    (b (cadr letters)))
               (set! the-str (partner the-str a b)))))
;          (printf "                            after: '~a'~n" the-str)
          ))
   (list->string the-str)))

(check-equal? (process "abcde" test-data) "baedc")

(define (part-one filename)
  (let ((commands (string-split (car (file->lines filename)) ",")))
   (process "abcdefghijklmnop" commands)))

(check-equal? (part-one "day16-data.txt") "hmefajngplkidocb")

;
; --- Part Two ---
;
; Now that you're starting to get a feel for the dance moves, you turn
;  your attention to the dance as a whole.
;
; Keeping the positions they ended up in from their previous dance,
;  the programs perform it again and again: including the first dance,
;  a total of one billion (1000000000) times.
;
; In the example above, their second dance would begin with the order
;  baedc, and use the same dance moves:
;
; * s1, a spin of size 1: cbaed.
; * x3/4, swapping the last two programs: cbade.
; * pe/b, swapping programs e and b: ceadb.
;
; In what order are the programs standing after their billion dances?
;  (Answer: "fbidepghmjklcnoa")
;

(define (part-two filename)
  (let ((commands (string-split (car (file->lines filename)) ","))
        (positions "abcdefghijklmnop"))
    (for ((i (range 2000)))
         (printf "~a~n" positions)
         (set! positions (process positions commands)))
   positions))

; I realized that with only 16 characters, there was probably cycling
; going on when you do a Billion rounds.  So I printed out a few thousand
; and found that indeed the numbers DO cycle, every 48 groups.

; So then I just had to find the remainder of a billion and 48 (16) and
; figure out which of the numbers (the 17th) represented the group I wanted.

; Not: dlfjbkamgcihnpoe ((48 * 20833333) + 16)
; Yes: fbidepghmjklcnoa ((48 * 20833333) + 17)

