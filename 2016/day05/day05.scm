
(require rackunit)
(require file/md5)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 5: How About a Nice Game of Chess? ---
;
; You are faced with a security door designed by Easter Bunny engineers that
;  seem to have acquired most of their security knowledge by watching hacking
;  movies.
;
; The eight-character password for the door is generated one character at a
;  time by finding the MD5 hash of some Door ID (your puzzle input) and an
;  increasing integer index (starting with 0).
;
; A hash indicates the next character in the password if its hexadecimal
;  representation starts with five zeroes. If it does, the sixth character
;  in the hash is the next character of the password.
;
; For example, if the Door ID is abc:
;
; * The first index which produces a hash that starts with five zeroes is 3231929,
;    which we find by hashing abc3231929; the sixth character of the hash, and
;    thus the first character of the password, is 1.
;
; * 5017308 produces the next interesting hash, which starts with 000008f82...,
;    so the second character of the password is 8.
;
; * The third time a hash starts with five zeroes is for abc5278568, discovering
;    the character f.
;
; In this example, after continuing this search a total of eight times, the password
;  is 18f47a30.
;
; Given the actual Door ID, what is the password? (Answer: "f77a0e6e")
;

(check-equal? (md5 #"abc3231929") #"00000155f8105dff7f56ee10fa9b9abd") ; Note 5 leading zeros

(define (part-one door-id)
  (let loop ((i 0) (cnt 0))
   (let* ((input (string-append door-id (number->string i)))
          (code (bytes->string/latin-1 (md5 input)))
          (first-five (substring code 0 5))
          (sixth (string-ref code 5)))
     (when (string=? "00000" first-five)
       (printf "Door Id: ~a, i: ~a, input: '~a', code: ~a, first five: '~a', sixth: '~a'~n" door-id i input code first-five sixth)
       (set! cnt (add1 cnt)))
     (when (= cnt 8) (error "That's enough!"))
     (loop (add1 i) cnt)))) ; f 7 7 a 0 e 6 e

;
; --- Part Two ---
;
; As the door slides open, you are presented with a second door that uses a slightly
;  more inspired security mechanism. Clearly unimpressed by the last version (in what
;  movie is the password decrypted in order?!), the Easter Bunny engineers have worked
;  out a better solution.
;
; Instead of simply filling in the password from left to right, the hash now also
;  indicates the position within the password to fill. You still look for hashes
;  that begin with five zeroes; however, now, the sixth character represents the
;  position (0-7), and the seventh character is the character to put in that position.
;
; A hash result of 000001f means that f is the second character in the password. Use
;  only the first result for each position, and ignore invalid positions.
;
; For example, if the Door ID is abc:
;
; * The first interesting hash is from abc3231929, which produces 0000015...; so, 5
;    goes in position 1: _5______.
;
; * In the previous method, 5017308 produced an interesting hash; however, it is
;    ignored, because it specifies an invalid position (8).
;
; * The second interesting hash is at index 5357525, which produces 000004e...;
;    so, e goes in position 4: _5__e___.
;
; You almost choke on your popcorn as the final character falls into place, producing
;  the password 05ace8e3.
;
; Given the actual Door ID and this new method, what is the password? Be extra proud
;  of your solution if it uses a cinematic "decrypting" animation. (Answer: "999828ec")
;

(define (part-two door-id)
  (let ((holder (make-vector 8 #\space)))
   (let loop ((i 0))
    (let* ((input (string-append door-id (number->string i)))
           (code (bytes->string/latin-1 (md5 input)))
           (first-five (substring code 0 5))
           (position (string-ref code 5))
           (the-char (string-ref code 6)))
      (when (string=? "00000" first-five)
        (printf "Input: '~a', code: ~a, first five: '~a', position '~a' the-char: '~a'~n" input code first-five position the-char)
        (when (and (char<=? position #\7)
                   (char>=? position #\0)
                   (char=? (vector-ref holder (string->number (string position))) #\space))
          (vector-set! holder (string->number (string position)) the-char)
          (printf "Holder: ~a~n" holder)))
      (loop (add1 i))))))

