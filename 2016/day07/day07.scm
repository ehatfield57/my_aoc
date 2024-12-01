
(require rackunit)
(require srfi/1)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 7: Internet Protocol Version 7 ---
;
; While snooping around the local network of EBHQ, you compile a list of IP
;  addresses (they're IPv7, of course; IPv6 is much too limited). You'd like
;  to figure out which IPs support TLS (transport-layer snooping).
;
; An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.
;  An ABBA is any four-character sequence which consists of a pair of two
;  different characters followed by the reverse of that pair, such as xyyx or
;  abba. However, the IP also must not have an ABBA within any hypernet sequences,
;  which are contained by square brackets.
;
; For example:
;
; * abba[mnop]qrst supports TLS (abba outside square brackets).
; * abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even
;    though xyyx is outside square brackets).
; * aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters
;    must be different).
; * ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even
;    though it's within a larger string).
;
; How many IPs in your puzzle input support TLS? (Answer: 105)
;

(define (supports-tls? lst)
  (let ((ch1 (car lst))
        (ch2 (cadr lst))
        (ch3 (caddr lst))
        (in-brackets? #f)
        (got-abba #f)
        (but-in-brackets #f))
    (for ((ch4 (cdddr lst)))
         (when (char=? ch1 #\[) (set! in-brackets? #t))
         (when (char=? ch1 #\]) (set! in-brackets? #f))

         (when (and (char=? ch1 ch4) (char=? ch2 ch3) (not (char=? ch1 ch2)))
           (if in-brackets?  (set! but-in-brackets #t) (set! got-abba #t)))

         (set! ch1 ch2)
         (set! ch2 ch3)
         (set! ch3 ch4))
    (and got-abba (not but-in-brackets))))

(check-equal? (supports-tls? (string->list "abba[mnop]qrst")) #t)
(check-equal? (supports-tls? (string->list "abcd[bddb]xyyx")) #f)
(check-equal? (supports-tls? (string->list "aaaa[qwer]tyui")) #f)
(check-equal? (supports-tls? (string->list "ioxxoj[asdfgh]zxcvbn")) #t)

(define (part-one filename)
  (let ((count 0)
        (lines (file->lines filename)))
    (for ((line lines))
         (when (supports-tls? (string->list line)) (set! count (add1 count))))
    count))

(printf "Part One: ~a~n" (part-one "day07-data.txt"))

;
; --- Part Two ---
; You would also like to know which IPs support SSL (super-secret listening).
;
; An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in
;  the supernet sequences (outside any square bracketed sections), and a
;  corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences.
;  An ABA is any three-character sequence which consists of the same character
;  twice with a different character between them, such as xyx or aba. A corresponding
;  BAB is the same characters but in reversed positions: yxy and bab, respectively.
;
; For example:
;
; * aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab
;    within square brackets).
; * xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
; * aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet;
;    the aaa sequence is not related, because the interior character must be different).
; * zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a corresponding
;    bzb, even though zaz and zbz overlap).
;
; How many IPs in your puzzle input support SSL? (Answer: 258)
;

(define (extract-aba codes lst)
  (let ((ch1 (car lst))
        (ch2 (cadr lst)))
    (for ((ch3 (cddr lst)))
         (when (and (char=? ch1 ch3) (not (char=? ch1 ch2)))
           (hash-set! codes (string ch1 ch2 ch1) (string ch2 ch1 ch2)))
         (set! ch1 ch2)
         (set! ch2 ch3))))

(define (process-line line)
  (let ((parts (regexp-match* #px"([\\w]+)" line #:match-select car))
         (brackets? #f)
         (aba (make-hash))
         (bab (make-hash)))
    (for ((part parts))
         (extract-aba (if brackets? bab aba) (string->list part))
         (set! brackets? (not brackets?)))
    (fold (lambda (x acc)
            (or acc (hash-has-key? bab (hash-ref aba x))))
          #f (hash-keys aba))))

(define (part-two filename)
  (let ((count 0)
        (lines (file->lines filename)))
    (for ((line lines))
         (when (eqv? #t (process-line line)) (set! count (add1 count))))
    count))

(check-equal? (process-line "aba[bab]xyz") #t)
(check-equal? (process-line "xyx[xyx]xyx") #f)
(check-equal? (process-line "aaa[kek]eke") #t)
(check-equal? (process-line "zazbz[bzb]cdb") #t)

(printf "Part Two: ~a~n" (part-two "day07-data.txt"))

