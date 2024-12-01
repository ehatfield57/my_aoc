
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 9: Stream Processing ---
;
; A large stream blocks your path. According to the locals, it's not safe
;  to cross the stream at the moment because it's full of garbage. You
;  look down at the stream; rather than water, you discover that it's a
;  stream of characters.
;
; You sit for a while and record part of the stream (your puzzle input).
;  The characters represent groups - sequences that begin with { and end
;  with }. Within a group, there are zero or more other things, separated
;  by commas: either another group or garbage. Since groups can contain
;  other groups, a } only closes the most-recently-opened unclosed group
;  - that is, they are nestable. Your puzzle input represents a single,
;  large group which itself contains many smaller ones.
;
; Sometimes, instead of a group, you will find garbage. Garbage begins with
;  < and ends with >. Between those angle brackets, almost any character can
;  appear, including { and }. Within garbage, < has no special meaning.
;
; In a futile attempt to clean up the garbage, some program has canceled
;  some of the characters within it using !: inside garbage, any character
;  that comes after ! should be ignored, including <, >, and even another !.
;
; You don't see any characters that deviate from these rules. Outside garbage,
;  you only find well-formed groups, and garbage always terminates according
;  to the rules above.
;
; Here are some self-contained pieces of garbage:
;
; * <>, empty garbage.
; * <random characters>, garbage containing random characters.
; * <<<<>, because the extra < are ignored.
; * <{!>}>, because the first > is canceled.
; * <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
; * <!!!>>, because the second ! and the first > are canceled.
; * <{o"i!a,<{i<a>, which ends at the first >.
;
; Here are some examples of whole streams and the number of groups they contain:
;
; * {}, 1 group.
; * {{{}}}, 3 groups.
; * {{},{}}, also 3 groups.
; * {{{},{},{{}}}}, 6 groups.
; * {<{},{},{{}}>}, 1 group (which itself contains garbage).
; * {<a>,<a>,<a>,<a>}, 1 group.
; * {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
; * {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).
;
; Your goal is to find the total score for all groups in your input. Each group
;  is assigned a score which is one more than the score of the group that immediately
;  contains it. (The outermost group gets a score of 1.)
;
; * {}, score of 1.
; * {{{}}}, score of 1 + 2 + 3 = 6.
; * {{},{}}, score of 1 + 2 + 2 = 5.
; * {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
; * {<a>,<a>,<a>,<a>}, score of 1.
; * {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
; * {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
; * {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
;
; What is the total score for all groups in your input? (Answer: 17537)
;

(define (group-cnt input-str)
  (define (helper input)
    (let ((ch (read-char input)))
     (cond
       ((eof-object? ch) 0)
       ((char=? #\{ ch) (+ 1 (helper input)))
       (else (+ 0 (helper input))))))
  (helper (open-input-string input-str)))

(define (score-str input-str)
  (define (helper input level)
    (let ((ch (read-char input)))
;     (printf "Hi Edward A, ch: '~a', level: ~a~n" ch level)
     (cond
       ((eof-object? ch) 0)
       ((char=? #\{ ch) (+ level (helper input (add1 level))))
       ((char=? #\} ch) (+ 0 (helper input (sub1 level))))
       (else (+ 0 (helper input level))))))
  (helper (open-input-string input-str) 1))

(define (clean-stream input-str)
  (let ((input (open-input-string input-str))
        (output (open-output-string))
        (garbage #f))
    (let loop ((ch (read-char input)))
     (unless (eof-object? ch)
       (cond
         ((char=? ch #\!) (read-char input))
         ((char=? ch #\<) (set! garbage #t))
         ((char=? ch #\>) (set! garbage #f))
         ((eq? garbage #f) (display ch output)))
       (loop (read-char input))))
    (get-output-string output)))

(define (part-one filename)
  (let ((input-str (file->string filename)))
   (score-str (clean-stream input-str))))

(check-equal? (score-str (clean-stream "{}")) 1)
(check-equal? (score-str (clean-stream "{{{}}}")) 6)
(check-equal? (score-str (clean-stream "{{},{}}")) 5)
(check-equal? (score-str (clean-stream "{{{},{},{{}}}}")) 16)
(check-equal? (score-str (clean-stream "{<a>,<a>,<a>,<a>}")) 1)
(check-equal? (score-str (clean-stream "{{<ab>},{<ab>},{<ab>},{<ab>}}")) 9)
(check-equal? (score-str (clean-stream "{{<!!>},{<!!>},{<!!>},{<!!>}}")) 9)
(check-equal? (score-str (clean-stream "{{<a!>},{<a!>},{<a!>},{<ab>}}")) 3)

(check-equal? (group-cnt (clean-stream "{}")) 1)
(check-equal? (group-cnt (clean-stream "{{{}}}")) 3)
(check-equal? (group-cnt (clean-stream "{{},{}}")) 3)
(check-equal? (group-cnt (clean-stream "{{{},{},{{}}}}")) 6)
(check-equal? (group-cnt (clean-stream "{<{},{},{{}}>}")) 1)
(check-equal? (group-cnt (clean-stream "{<a>,<a>,<a>,<a>}")) 1)
(check-equal? (group-cnt (clean-stream "{{<a>},{<a>},{<a>},{<a>}}")) 5)
(check-equal? (group-cnt (clean-stream "{{<!>},{<!>},{<!>},{<a>}}")) 2)

(part-one "day09-data.txt")

;
; --- Part Two ---
;
; Now, you're ready to remove the garbage.
;
; To prove you've removed it, you need to count all of the characters within
;  the garbage. The leading and trailing < and > don't count, nor do any canceled
;  characters or the ! doing the canceling.
;
; * <>, 0 characters.
; * <random characters>, 17 characters.
; * <<<<>, 3 characters.
; * <{!>}>, 2 characters.
; * <!!>, 0 characters.
; * <!!!>>, 0 characters.
; * <{o"i!a,<{i<a>, 10 characters.
;
; How many non-canceled characters are within the garbage in your puzzle input?
; (Answer: 7539)
;

(define (count-garbage input-str)
  (let ((input (open-input-string input-str))
        (garbage #f)
        (total 0))
    (let loop ((ch (read-char input)))
     (unless (eof-object? ch)
       (cond
         ((char=? ch #\!) (read-char input))
         ((char=? ch #\<)
          (if (not garbage)
              (set! garbage #t)
              (set! total (add1 total))))
         ((char=? ch #\>) (set! garbage #f))
         ((eq? garbage #t) (set! total (add1 total))))
       (loop (read-char input))))
    total))

(check-equal? (count-garbage "<>") 0)
(check-equal? (count-garbage "<random characters>") 17)
(check-equal? (count-garbage "<<<<>") 3)
(check-equal? (count-garbage "<{!>}>") 2)
(check-equal? (count-garbage "<!!>") 0)
(check-equal? (count-garbage "<!!!>>") 0)
(check-equal? (count-garbage "<{o'i!a,<{i<a>") 10)

(define (part-two filename)
  (count-garbage (file->string filename)))

(part-two "day09-data.txt")

