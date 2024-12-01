
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 14: One-Time Pad ---
;
; In order to communicate securely with Santa while you're on this mission,
;  you've been using a one-time pad that you generate using a pre-agreed
;  algorithm. Unfortunately, you've run out of keys in your one-time pad,
;  and so you need to generate some more.
;
; To generate keys, you first get a stream of random data by taking the MD5
;  of a pre-arranged salt (your puzzle input) and an increasing integer index
;  (starting with 0, and represented in decimal); the resulting MD5 hash should
;  be represented as a string of lowercase hexadecimal digits.
;
; However, not all of these MD5 hashes are keys, and you need 64 new keys for
;  your one-time pad. A hash is a key only if:
;
; * It contains three of the same character in a row, like 777. Only consider
;    the first such triplet in a hash.
;
; * One of the next 1000 hashes in the stream contains that same character five
;    times in a row, like 77777.
;
; Considering future hashes for five-of-a-kind sequences does not cause those
;  hashes to be skipped; instead, regardless of whether the current hash is a key,
;  always resume testing for keys starting with the very next hash.
;
; For example, if the pre-arranged salt is abc:
;
; * The first index which produces a triple is 18, because the MD5 hash of abc18
;    contains ...cc38887a5.... However, index 18 does not count as a key for your
;    one-time pad, because none of the next thousand hashes (index 19 through index
;    1018) contain 88888.
;
; * The next index which produces a triple is 39; the hash of abc39 contains eee.
;    It is also the first key: one of the next thousand hashes (the one at index
;    816) contains eeeee.
;
; * None of the next six triples are keys, but the one after that, at index 92, is:
;    it contains 999 and index 200 contains 99999.
;
; * Eventually, index 22728 meets all of the criteria to generate the 64th key.
;
; So, using our example salt of abc, index 22728 produces the 64th key.
;
; Given the actual salt in your puzzle input, what index produces your 64th
;  one-time pad key? (Answer: 23769)
;

(require file/md5)

(define test-input "abc")
(define puzzle-input "cuanljph")

(define (encode secret-key n)  ; -> md5-string
  (bytes->string/latin-1 (md5 (string-append secret-key (number->string n)))))

(define (has-5-or-3 str)  ; -> (0 . "") or (3 . "x") or (5 . "x")
  (let* ((match-5 (regexp-match* #px"(.)\\1\\1\\1\\1" str #:match-select cdr))
         (match-3 (regexp-match* #px"(.)\\1\\1" str #:match-select cdr))
         (match-5? (not (null? match-5)))
         (match-3? (not (null? match-3)))
         (match-char (if (or match-5? match-3?) (car (car match-3)) "")))
    (cond
      ((and match-5? match-3?) (cons 5 match-char))
      (match-3?                (cons 3 match-char))
      (else                    (cons 0 match-char)))))

(define (stretch salt idx)
  (let ((the-md5 (encode salt idx)))
   (for ((n (range 2016)))
        (set! the-md5 (bytes->string/latin-1 (md5 the-md5))))
   the-md5))

(define fiver-top-index 0)
(define (find-fivers limit salt)
  (let ((cnt 0)
        (fivers (make-hash)))
   (let loop ((idx 0))
    (let* ((value (encode salt idx)) ; Part One
;    (let* ((value (stretch salt idx)) ; Part Two
           (combo (has-5-or-3 value))
           (has-n (car combo))
           (has-c (cdr combo)))
      (when (= has-n 5)
        (if (hash-has-key? fivers has-c)
            (hash-set! fivers has-c (cons idx (hash-ref fivers has-c)))
            (hash-set! fivers has-c (list idx)))
        (set! cnt (add1 cnt))
        (set! fiver-top-index idx))
      (when (< cnt limit) (loop (add1 idx)))))
   (printf "Hi Edward, ~a Fivers generated! Top index: ~a~n" limit fiver-top-index)
   fivers))

(define (find-64 salt)
  (let ((one-time-pad '())
        (fivers (find-fivers 100 salt)))
   (let loop ((i 0))
    (when (> i fiver-top-index) (error "Went over fiver-top-index!"))
    (let* ((value (encode salt i)) ; Part One
;    (let* ((value (stretch salt i)) ; Part Two
           (combo (has-5-or-3 value))
           (has-n (car combo))
           (has-c (cdr combo)))
      (when (and (= has-n 3) (hash-has-key? fivers has-c))
        (let* ((fiver-locations (hash-ref fivers has-c))
               (in-a-thousand (lambda (x) (and (> x i) (< x (+ i 1000)))))
               (within-range (filter in-a-thousand fiver-locations)))
          (when (not (zero? (length within-range)))
            (set! one-time-pad (cons value one-time-pad))
            (printf "Added index ~a with value: ~a~n" i value))
         )))
    (if (>= (length one-time-pad) 64)
        (reverse one-time-pad)
        (loop (add1 i))))))

;
; --- Part Two ---
;
; Of course, in order to make this process even more secure, you've also
;  implemented key stretching.
;
; Key stretching forces attackers to spend more time generating hashes.
;  Unfortunately, it forces everyone else to spend more time, too.
;
; To implement key stretching, whenever you generate a hash, before you
;  use it, you first find the MD5 hash of that hash, then the MD5 hash
;  of that hash, and so on, a total of 2016 additional hashings. Always
;  use lowercase hexadecimal representations of hashes.
;
; For example, to find the stretched hash for index 0 and salt abc:
;
; * Find the MD5 hash of abc0: 577571be4de9dcce85a041ba0410f29f.
;
; * Then, find the MD5 hash of that hash: eec80a0c92dc8a0777c619d9bb51e910.
;
; * Then, find the MD5 hash of that hash: 16062ce768787384c81fe17a7a60c7e3.
;
; * ...repeat many times...
;
; * Then, find the MD5 hash of that hash: a107ff634856bb300138cac6568c0f24.
;
; So, the stretched hash for index 0 in this situation is a107ff.... In the
;  end, you find the original hash (one use of MD5), then find the hash-of-
;  the-previous-hash 2016 times, for a total of 2017 uses of MD5.
;
; The rest of the process remains the same, but now the keys are entirely
;  different. Again for salt abc:
;
; * The first triple (222, at index 5) has no matching 22222 in the next
;    thousand hashes.
;
; * The second triple (eee, at index 10) hash a matching eeeee at index 89,
;    and so it is the first key.
;
; * Eventually, index 22551 produces the 64th key (triple fff with matching
;    fffff at index 22859.
;
; Given the actual salt in your puzzle input and using 2016 extra MD5 calls
;  of key stretching, what index now produces your 64th one-time pad key?
;  (Answer: 20606)
;

; My first attempt was 20683 and is too high. :-(
; Taking the 2nd from the last '20606' worked! :-)

