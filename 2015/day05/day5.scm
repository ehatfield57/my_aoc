
(require rackunit)

; Advent of Code - 2015
; 
; --- Day 5: Doesn't He Have Intern-Elves For This? ---
;
; --- Part One ---
;
; Santa needs help figuring out which strings in his text file are naughty or nice.
; 
; A nice string is one with all of the following properties:
; 
; * It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
; * It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
; * It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

; For example:
; 
; * ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...),
;  and none of the disallowed substrings.
;
; * aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
;
; * jchzalrnumimnmhp is naughty because it has no double letter.
;
; * haegwjzuvuyypxyu is naughty because it contains the string xy.
;
; * dvszwmarrgswjxmb is naughty because it contains only one vowel.
;
; How many strings are nice? (Answer: 258)

(define vowels (lambda (str) (regexp-match* #rx"([aeiou])" str)))
(define three-vowels-check (lambda (str) (>= (length (vowels str)) 3)))

(check-equal? (three-vowels-check "aei") #t)
(check-equal? (three-vowels-check "xazegov") #t)
(check-equal? (three-vowels-check "aeiouaeiouaeiou") #t)


(define double-letter (lambda (str) (regexp-match* #px"([a-zA-Z])\\1" str)))
(define double-letter-check (lambda (str) (>= (length (double-letter str)) 1)))

(check-equal? (double-letter-check "xx") #t)
(check-equal? (double-letter-check "abcdde") #t)
(check-equal? (double-letter-check "aabbccdd") #t)


(define make-contains
  (lambda (str)
    (lambda (substr)
      (if (regexp-match (regexp substr) str) #t #f))))

(define check-for-substrings
  (lambda (str lst-substrings)
    (let ((contain (make-contains str))
          (result #f))
      (for-each (lambda (substr) (and (contain substr) (set! result #t))) lst-substrings)
      result)))

(define not-contain-strings
  (lambda (str)
    (not (check-for-substrings str '("ab" "cd" "pq" "xy")))))

(check-equal? (not-contain-strings "xabx") #f)
(check-equal? (not-contain-strings "cd") #f)
(check-equal? (not-contain-strings "pqx") #f)
(check-equal? (not-contain-strings "xxy") #f)
(check-equal? (not-contain-strings "lmnope") #t)


(define nice
  (lambda (str)
    (and (three-vowels-check str)
         (double-letter-check str)
         (not-contain-strings str))))

(check-equal? (nice "ugknbfddgicrmopn") #t)
(check-equal? (nice "aaa") #t)
(check-equal? (nice "jchzalrnumimnmhp") #f)
(check-equal? (nice "haegwjzuvuyypxyu") #f)
(check-equal? (nice "dvszwmarrgswjxmb") #f)


(define part-one
  (lambda ()
    (let ((nice-cnt 0))
     (with-input-from-file
       "day5-data.txt"
       (lambda ()
         (for ([line (in-lines)])
           (and (nice line) (set! nice-cnt (add1 nice-cnt))))))
     nice-cnt)))

(check-equal? (part-one) 258)


; --- Part Two ---
; 
; Realizing the error of his ways, Santa has switched to a better model of determining
;  whether a string is naughty or nice. None of the old rules apply, as they are all
;  clearly ridiculous.
; 
; Now, a nice string is one with all of the following properties:
; 
; * It contains a pair of any two letters that appears at least twice in the string
;    without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
;
; * It contains at least one letter which repeats with exactly one letter between them,
;    like xyx, abcdefeghi (efe), or even aaa.
; 
; For example:
; 
; * qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and
;    a letter that repeats with exactly one letter between them (zxz).
;
; * xxyxx is nice because it has a pair that appears twice and a letter that
;    repeats with one between, even though the letters used by each rule overlap.
;
; * uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a
;    single letter between them.
;
; * ieodomkazucvgmuy is naughty because it has a repeating letter with one between
;    (odo), but no pair that appears twice.
; 
; How many strings are nice under these new rules? (Answer: 53)

(define any-two-letter-pair
  (lambda (str)
    (> (length (regexp-match* #px"([a-zA-Z][a-zA-Z]).*\\1" str)) 0)))

(check-equal? (any-two-letter-pair "xyxy") #t)
(check-equal? (any-two-letter-pair "aabcdefgaa") #t)
(check-equal? (any-two-letter-pair "aaa") #f)


(define sandwiched-set
  (lambda (str)
    (> (length (regexp-match* #px"([a-zA-Z]).\\1" str)) 0)))

(check-equal? (sandwiched-set "xyx") #t)
(check-equal? (sandwiched-set "abcdefeghi") #t)
(check-equal? (sandwiched-set "aaa") #t)
(check-equal? (sandwiched-set "abc") #f)


(define nice2
  (lambda (str)
    (and (any-two-letter-pair str)
         (sandwiched-set str))))

(check-equal? (nice2 "qjhvhtzxzqqjkmpb") #t)
(check-equal? (nice2 "xxyxx") #t)
(check-equal? (nice2 "uurcxstgmygtbstg") #f)
(check-equal? (nice2 "ieodomkazucvgmuy") #f)


(define part-two
  (lambda ()
    (let ((nice-cnt 0))
     (with-input-from-file
       "day5-data.txt"
       (lambda ()
         (for ([line (in-lines)])
           (and (nice2 line) (set! nice-cnt (add1 nice-cnt))))))
     nice-cnt)))

(check-equal? (part-two) 53)

