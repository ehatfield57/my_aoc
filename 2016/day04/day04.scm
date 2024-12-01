
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 4: Security Through Obscurity ---
;
; Finally, you come across an information kiosk with a list of rooms. Of
;  course, the list is encrypted and full of decoy data, but the instructions
;  to decode the list are barely hidden nearby. Better remove the decoy data
;  first.
;
; Each room consists of an encrypted name (lowercase letters separated by
;  dashes) followed by a dash, a sector ID, and a checksum in square brackets.
;
; A room is real (not a decoy) if the checksum is the five most common letters
;  in the encrypted name, in order, with ties broken by alphabetization.
;  For example:
;
; * aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters
;    are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
;
; * a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied
;    (1 of each), the first five are listed alphabetically.
;
; * not-a-real-room-404[oarel] is a real room.
;
; * totally-real-room-200[decoy] is not.
;
; Of the real rooms from the list above, the sum of their sector IDs is 1514.
;
; What is the sum of the sector IDs of the real rooms? (Answer: 409147)
;

(define test-data '(
                    "aaaaa-bbb-z-y-x-123[abxyz]"
                    "a-b-c-d-e-f-g-h-987[abcde]"
                    "not-a-real-room-404[oarel]"
                    "totally-real-room-200[decoy]"))

(define (parts str)
  (cdr (car (regexp-match* #rx"^(.*)-([0-9]+).([^]]+)]" str #:match-select values))))

(define (tally str)
  (sort (string->list (regexp-replace* #rx"-" str "")) char<?))

(define (freq lst) ; (list of char) -> '((#\a . 5) (#\b . 3) (#\z . 1) (#\y . 1) (#\x . 1)) 
  (let ((frq (make-hash)))
   (let loop ((lis lst))
    (cond
      ((null? lis) (hash->list frq))
      (else
        (if (hash-has-key? frq (car lis))
            (hash-set! frq (car lis) (add1 (hash-ref frq (car lis))))
            (hash-set! frq (car lis) 1))
        (loop (cdr lis)))))))

(define (reverse-char c) ; #\a -> #\z (used by sort-key to reverse sort for char)
  (let* ((a (char->integer #\a))
         (z (char->integer #\z))
         (alpha-cnt (add1 (- z a)))
         (base (sub1 a)))
    (integer->char
      (+ 1 base
         (modulo
           (- z (char->integer c))
           alpha-cnt)))))

(define (sort-key p) ; p is (#\a . 4) -> "04z" (to sort desc count and asc char)
   (string-append
     (if (< (cdr p) 10) "0" "")
     (number->string (cdr p)) 
     (string (reverse-char (car p)))))

(define (sort-freq lst) ; hash -> list
  (reverse (sort lst (lambda (a b) (string<? (sort-key a) (sort-key b))))))

(define (make-checksum sorted-freq)
  (substring (string-append (apply string (map car sorted-freq))) 0 5))

(define (good-checksum line)
  (let* ((the-parts (parts line))
         (name (car the-parts))
         (checksum (caddr the-parts))
         (test-checksum  (make-checksum (sort-freq (freq (tally name))))))
    (string=? test-checksum checksum)))

(define (extract-id line)
  (string->number (cadr (parts line))))

(define (part-one data)
   (apply + (map extract-id (filter good-checksum data))))

(check-equal? #t (good-checksum (car test-data)))
(check-equal? #t (good-checksum (cadr test-data)))
(check-equal? #t (good-checksum (caddr test-data)))
(check-equal? #f (good-checksum (cadddr test-data)))
(check-equal? (part-one test-data) 1514)

(check-equal? (part-one (file->lines "day04-data.txt")) 409147)

;
; --- Part Two ---
;
; With all the decoy data out of the way, it's time to decrypt this list and get moving.
;
; The room names are encrypted by a state-of-the-art shift cipher, which is nearly
;  unbreakable without the right software. However, the information kiosk designers at
;  Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.
;
; To decrypt a room name, rotate each letter forward through the alphabet a number of
;  times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so
;  on. Dashes become spaces.
;
; For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
;
; What is the sector ID of the room where North Pole objects are stored? (Answer: 991)
;

(define (decode name sector) ; (str num) -> (list of chars)
  (let* ((a (char->integer #\a))
         (z (char->integer #\z))
         (alpha-cnt (add1 (- z a)))
         (base (sub1 a)))
    (map (lambda (c)
           (if (char=? #\- c)
               #\space
               (integer->char
                 (+ base
                    (modulo
                      (+ sector (- (char->integer c) base))
                      alpha-cnt)))))
         (string->list name))))

(define (decypher line)
  (let* ((the-parts (parts line))
         (name (car the-parts))
         (sector (string->number (cadr the-parts))))
    (list->string (decode name sector))))

(define (part-two data)
   (for ((line (filter good-checksum data)))
        (printf "'~a' -> '~a'~n" line (decypher line))))

(check-equal? (decypher "qzmt-zixmtkozy-ivhz-343[zimth]") "very encrypted name")

(part-two (file->lines "day04-data.txt"))

; line 554 -> 'kloqemlib-lygbzq-pqloxdb-991[lbqod]' -> 'northpole object storage'

