;
; Advent of Code - 2015
;
; --- Day 4: The Ideal Stocking Stuffer ---
;
; Part One
;
; Santa needs help mining some AdventCoins (very similar to bitcoins) to use
;  as gifts for all the economically forward-thinking little girls and boys.
;
; To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
;  least five zeroes. The input to the MD5 hash is some secret key (your puzzle
;  input, given below) followed by a number in decimal. To mine AdventCoins,
;  you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
;  that produces such a hash.
;
; For example:
;
; If your secret key is abcdef, the answer is 609043, because the MD5 hash of
;  abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
;  such number to do so.
;
; If your secret key is pqrstuv, the lowest number it combines with to make an
;  MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of
;  pqrstuv1048970 looks like 000006136ef....
;
; Your puzzle input is iwrupvqb. (Answer: 346386)

(require file/md5)

(define encode
  (lambda (secret-key n)
    (md5 (string-append secret-key (number->string n)))))

(define first
  (lambda (n md5-bstr)
    (let ((str '()))
     (for ([i (in-range 0 n)])
       (set! str (cons (integer->char (bytes-ref md5-bstr i)) str)))
     (apply string str))))

(or (string=? (make-string 5 #\0) (first 5 (encode "abcdef" 609043))) (error "The first 5 characters are not zeros"))
(displayln "First test passed")
(or (string=? (make-string 5 #\0) (first 5 (encode "pqrstuv" 1048970))) (error "The first 5 characters are not zeros"))
(displayln "Second test passed")

(define find-number
  (lambda (zero-cnt code)
    (do ((n 1 (+ n 1)))
        ((string=? (make-string zero-cnt #\0) (first zero-cnt (encode code n))) n))))

(or (= 609043 (find-number 5 "abcdef")) (error "Example with code 'abcdef' failed"))
(or (= 1048970 (find-number 5 "pqrstuv")) (error "Example with code 'pqrstuv' failed"))

(displayln (string-append "The Answer 5 zeros is: " (number->string (find-number 5 "iwrupvqb")))) ; 346386

; --- Part Two ---
;
; Now find one that starts with six zeroes. (Answer: 9958218)

(displayln (string-append "The Answer with 6 zeros is: " (number->string (find-number 6 "iwrupvqb")))) ; 9958218

