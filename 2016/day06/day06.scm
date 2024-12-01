
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 6: Signals and Noise ---
;
; Something is jamming your communications with Santa. Fortunately, your signal
;  is only partially jammed, and protocol in situations like this is to switch
;  to a simple repetition code to get the message through.
;
; In this model, the same message is sent repeatedly. You've recorded the repeating
;  message signal (your puzzle input), but the data seems quite corrupted - almost
;  too badly to recover. Almost.
;
; All you need to do is figure out which character is most frequent for each position.
;  For example, suppose you had recorded the following messages:
;
; eedadn
; drvtee
; eandsr
; raavrd
; atevrs
; tsrnev
; sdttsa
; rasrtv
; nssdts
; ntnada
; svetve
; tesnvt
; vntsnd
; vrdear
; dvrsen
; enarar
;
; The most common character in the first column is e; in the second, a; in the third,
;  s, and so on. Combining these characters returns the error-corrected message, easter.
;
; Given the recording in your puzzle input, what is the error-corrected version of the
;  message being sent? (Answer: "usccerug")
;

(define test-data '( "eedadn" "drvtee" "eandsr" "raavrd" "atevrs" "tsrnev" "sdttsa" "rasrtv"
                     "nssdts" "ntnada" "svetve" "tesnvt" "vntsnd" "vrdear" "dvrsen" "enarar"))

(define (accumulator col-num)
  (let ((freq (make-hash)))
   (lambda (cmd . arg)
     (case cmd
       ('add
        (define the-char (string-ref (car arg) col-num))
        (if (hash-has-key? freq the-char)
            (hash-set! freq the-char (add1 (hash-ref freq the-char)))
            (hash-set! freq the-char 1)))
       ('max
        (caar (sort (hash->list freq) (lambda (a b) (< (cdr b) (cdr a))))))
       ('min
        (caar (sort (hash->list freq) (lambda (a b) (> (cdr b) (cdr a))))))
       ('show
        (printf "Accumulator ~a: ~a~n" col-num freq))
       (else
         (printf "Unknown accumulator command: '~a'~n" cmd))))))

(define (part-one size data)
  (let ((accumulators (make-vector size)))
   (for ((i (range size)))
        (vector-set! accumulators i (accumulator i)))
   (for-each (lambda (str)
               (for ((i (range size))) ((vector-ref accumulators i) 'add str)))
             data)
   ;(map (lambda (acc) (acc 'show)) (vector->list accumulators))
   
   (string-join (map (lambda (acc) (string (acc 'max))) (vector->list accumulators)) "")))

(check-equal? (part-one 6 test-data) "easter")

(check-equal?  (part-one 8 (file->lines "day06-data.txt")) "usccerug")

;
; --- Part Two ---
;
; Of course, that would be the message - if you hadn't agreed to use a modified repetition
;  code instead.
;
; In this modified code, the sender instead transmits what looks like random data, but for
;  each character, the character they actually want to send is slightly less likely than the
;  others. Even after signal-jamming noise, you can look at the letter distributions in each
;  column and choose the least common letter to reconstruct the original message.
;
; In the above example, the least common character in the first column is a; in the second,
;  d, and so on. Repeating this process for the remaining characters produces the original
;  message, advent.
;
; Given the recording in your puzzle input and this new decoding methodology, what is the
;  original message that Santa is trying to send? (Answer: ?)
;

(define (part-two size data)
  (let ((accumulators (make-vector size)))
   (for ((i (range size)))
        (vector-set! accumulators i (accumulator i)))
   (for-each (lambda (str)
               (for ((i (range size))) ((vector-ref accumulators i) 'add str)))
             data)
   (string-join (map (lambda (acc) (string (acc 'min))) (vector->list accumulators)) "")))

(check-equal? (part-two 6 test-data) "advent")

(check-equal? (part-two 8 (file->lines "day06-data.txt")) "cnvvtafc")

