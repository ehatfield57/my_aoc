
(require rackunit)
(require racket/trace)
(require racket/generator)

;
; Advent of Code 2015
;
; Part One
;
; --- Day 11: Corporate Policy ---
;
; Santa's previous password expired, and he needs help choosing a new one.
;
; To help him remember his new password after the old one expires, Santa
;  has devised a method of coming up with a password based on the previous
;  one. Corporate policy dictates that passwords must be exactly eight
;  lowercase letters (for security reasons), so he finds his new password
;  by incrementing his old password string repeatedly until it is valid.
;
; Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and
;  so on. Increase the rightmost letter one step; if it was z, it wraps
;  around to a, and repeat with the next letter to the left until one doesn't
;  wrap around.
;
; Unfortunately for Santa, a new Security-Elf recently started, and he has
;  imposed some additional password requirements:
;
; * Passwords must include one increasing straight of at least three letters,
;    like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd
;    doesn't count.
;
; * Passwords may not contain the letters i, o, or l, as these letters can be
;    mistaken for other characters and are therefore confusing.
;
; * Passwords must contain at least two different, non-overlapping pairs of
;    letters, like aa, bb, or zz.
;
; For example:
;
; * hijklmmn meets the first requirement (because it contains the straight hij)
;    but fails the second requirement requirement (because it contains i and l).
;
; * abbceffg meets the third requirement (because it repeats bb and ff) but
;    fails the first requirement.
;
; * abbcegjk fails the third requirement, because it only has one double letter (bb).
;
; * The next password after abcdefgh is abcdffaa.
;
; * The next password after ghijklmn is ghjaabcc, because you eventually skip all
;    the passwords that start with ghi..., since i is not allowed.
;
; Given Santa's current password (your puzzle input), what should his next password be?
;
; Your puzzle input is hxbxwxba.  (Answer: "hxbxxyzz")
;

(define (inc-str str)
  (define first-char #\a)
  (define last-char #\z)
  (define char-inc (lambda (ch) (integer->char (add1 (char->integer ch)))))
  (define (helper char-lst carry)
    (cond
      ((= 1 carry)
       (cond
         ((null? char-lst) (list first-char))
         ((char=? last-char (car char-lst))
          (cons first-char (helper (cdr char-lst) 1)))
         (else (cons (char-inc (car char-lst)) (helper (cdr char-lst) 0)))))
      (else
        (cond
          ((null? char-lst) '())
          (else (cons (car char-lst) (helper (cdr char-lst) 0)))))))
  (list->string (reverse (helper (reverse (string->list str)) 1))))

(define g
  (generator ()
             (do ((result (inc-str "") (inc-str result)))
                 (#f) (yield result))))

(check-equal? (g) "a")
(for ((i (in-range 1 25))) (g))
(check-equal? (g) "z")
(check-equal? (g) "aa")
(check-equal? (g) "ab")


;
; rule-zero - The password must be exactly 8 lower case letters.
;
(define (rule-zero str)
  (and (= 8 (string-length str)) (not (not (regexp-match #rx"^[a-z]+$" str)))))

(check-equal? (rule-zero "abcdef") #f)
(check-equal? (rule-zero "abcdefgh") #t)
(check-equal? (rule-zero "abcdeFgh") #f)


;
; rule-one - Passwords must include one increasing straight of at least three
;             letters, like abc, bcd, cde, and so on, up to xyz. They cannot
;             skip letters; abd doesn't count.
;
(define (rule-one str)
  (let ((found #f))
   (do ((i 0 (add1 i))
        (j 3 (add1 j)))
       ((> j (string-length str)) found)
       (when (string-contains? "abcdefghijklmnopqrstuvwxyz" (substring str i j))
         (set! found #t)))))

(check-equal? (rule-one "hijklmmn") #t)
(check-equal? (rule-one "abbceffg") #f)
(check-equal? (rule-one "abbcegjk") #f)


;
; rule-two - Passwords may not contain the letters i, o, or l, as these letters
;             can be mistaken for other characters and are therefore confusing.
;
(define (rule-two str) (not (regexp-match #rx"[iol]" str)))

(check-equal? (rule-two "hijklmmn") #f)
(check-equal? (rule-two "abbceffg") #t)
(check-equal? (rule-two "abbcegjk") #t)


;
; rule-three - Passwords must contain at least two different, non-overlapping
;               pairs of letters, like aa, bb, or zz.
;
(define (rule-three str) ; NOTE: There could be a bug here if there are three pairs
  (let ((pairs (regexp-match* #px"((.)\\2)" str))) ; and the 1st and 3rd match.
   (if (and (> (length pairs) 1) (not (string=? (car pairs) (cadr pairs))))
       #t #f)))

(check-equal? (rule-three "hijklmmn") #f)
(check-equal? (rule-three "abbceffg") #t)
(check-equal? (rule-three "abbcegjk") #f)


(define (good str)
  (and (rule-zero str) (rule-one str) (rule-two str) (rule-three str)))

(check-equal? (good "hijklmmn") #f)
(check-equal? (good "abbceffg") #f)
(check-equal? (good "abbcegjk") #f)

(check-equal? (good "abcdffaa") #t)
(check-equal? (good "ghjaabcc") #t)


;
; part-one -> "hxbxxyzz"
;
(define part-one-test-A
  (generator ()
             (do ((result (inc-str "abcdefgh") (inc-str result)))
                 (#f) (when (good result) (yield result)))))
(check-equal? (part-one-test-A) "abcdffaa")

(define part-one-test-B
  (generator ()
             (do ((result (inc-str "ghijklmn") (inc-str result)))
                 (#f) (when (good result) (yield result)))))
(check-equal? (part-one-test-B) "ghjaabcc")

(define part-one
  (generator ()
             (do ((result (inc-str "hxbxwxba") (inc-str result)))
                 (#f) (when (good result) (yield result)))))
(check-equal? (part-one) "hxbxxyzz")

;
; --- Part Two ---
;
; Santa's password expired again. What's the next one? (Answer: "hxcaabcc")
;
(check-equal? (part-one) "hxcaabcc")

