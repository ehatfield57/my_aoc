
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 4: High-Entropy Passphrases ---
;
; A new system policy has been put in place that requires all accounts
;  to use a passphrase instead of simply a password. A passphrase consists
;  of a series of words (lowercase letters) separated by spaces.
;
; To ensure security, a valid passphrase must contain no duplicate words.
;
; For example:
;
; aa bb cc dd ee is valid.
; aa bb cc dd aa is not valid - the word aa appears more than once.
; aa bb cc dd aaa is valid - aa and aaa count as different words.
;
; The system's full passphrase list is available as your puzzle input.
;  How many passphrases are valid? (Answer: 466)
;

(define (valid str)
  (if (regexp-match #px"\\b([a-z]+)\\b.*\\b\\1\\b" str) #f #t))

(check-equal? (valid "aa bb cc dd ee") #t)
(check-equal? (valid "aa bb cc dd aa") #f)
(check-equal? (valid "aa bb cc dd aaa") #t)

(define (part-one filename)
  (let ((lines (file->lines filename))
        (good 0))
   (for ((line lines))
        (when (valid line) (set! good (add1 good))))
   good))

(check-equal? (part-one "day04-data.txt") 466)

;
; --- Part Two ---
;
; For added security, yet another system policy has been put in place. Now, a
;  valid passphrase must contain no two words that are anagrams of each other
;  - that is, a passphrase is invalid if any word's letters can be rearranged
;  to form any other word in the passphrase.
;
; For example:
;
; * abcde fghij is a valid passphrase.
; * abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
; * a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
; * iiii oiii ooii oooi oooo is valid.
; * oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
;
; Under this new system policy, how many passphrases are valid? (Answer: 251)
;

(define (anagrams line) 
  (let ((words (string-split line))
        (ident (make-hash))
        (return #f))
    (for ((word words))
         (define key (list->string (sort (string->list word) char<?)))
         (if (hash-has-key? ident key)
             (set! return #t)
             (hash-set! ident key #t)))
    return))

(define (part-two filename)
  (let ((lines (file->lines filename))
        (good 0))
    (for ((line lines))
         (when (not (anagrams line)) (set! good (add1 good))))
    good))

(check-equal? (anagrams "abcde fghij") #f)
(check-equal? (anagrams "abcde xyz ecdab") #t)
(check-equal? (anagrams "a ab abc abd abf abj") #f)
(check-equal? (anagrams "iiii oiii ooii oooi oooo") #f)
(check-equal? (anagrams "oiii ioii iioi iiio") #t)

(check-equal? (part-two "day04-data.txt") 251)

