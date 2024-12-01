
(require rackunit)
(require racket/trace)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 9: Explosives in Cyberspace ---
;
; Wandering around a secure area, you come across a datalink port to a new part of the
;  network. After briefly scanning it for interesting files, you find one file in
;  particular that catches your attention. It's compressed with an experimental format,
;  but fortunately, the documentation for the format is nearby.
;
; The format compresses a sequence of characters. Whitespace is ignored. To indicate
;  that some sequence should be repeated, a marker is added to the file, like (10x2).
;  To decompress this marker, take the subsequent 10 characters and repeat them 2 times.
;  Then, continue reading the file after the repeated data. The marker itself is not
;  included in the decompressed output.
;
; If parentheses or other characters appear within the data referenced by a marker,
;  that's okay - treat it like normal data, not a marker, and then resume looking for
;  markers after the decompressed section.
;
; For example:
;
; * ADVENT contains no markers and decompresses to itself with no changes, resulting in
;    a decompressed length of 6.
; * A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed
;    length of 7.
; * (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
; * A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed
;    length of 11.
; * (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's
;     within a data section of another marker, it is not treated any differently from the
;     A that comes after it. It has a decompressed length of 6.
; * X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because
;    the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped and not processed
;    further.
;
; What is the decompressed length of the file (your puzzle input)? Don't count whitespace.
;  (Answer: 138735)
;

(define tests '(
                ("ADVENT"            . "ADVENT")
                ("A(1x5)BC"          . "ABBBBBC")
                ("(3x3)XYZ"          . "XYZXYZXYZ")
                ("A(2x2)BCD(2x2)EFG" . "ABCBCDEFEFG")
                ("(6x1)(1x3)A"       . "(1x3)A")
                ("X(8x2)(3x3)ABCY"   . "X(3x3)ABC(3x3)ABCY")))

(define (get-marker input)
  (let ((ch (read-string 1 input)))
   (if (string=? ch ")") '()
       (cons ch (get-marker input)))))

(define (process-marker marker-str input)
  (let ((temp-str (open-output-string))
        (return (open-output-string)))
    (define-values (_ len times) (apply values (car (regexp-match* #rx"([0-9]+)x([0-9]+)" marker-str #:match-select values))))
    (write-string (read-string (string->number len) input) temp-str)
    (for ((i (string->number times)))
         (write-string (get-output-string temp-str) return))
    (get-output-string return)))

(define (explode input)
  (let ((output (open-output-string)))
   (let loop ((ch (read-string 1 input)))
    (unless (eof-object? ch)
      (if (string=? ch "(")
          (let ((marker-str (apply string-append (get-marker input))))
           (write-string (process-marker marker-str input) output))
          (write-string ch output))
      (loop (read-string 1 input)))
    )
   (get-output-string output)))

(define (run-tests tests)
  (for ((test-and-response tests))
       (let ((input (open-input-string (car test-and-response))))
        (check-equal? (explode input) (cdr test-and-response)))))
(run-tests tests)

(define (part-one filename)
  (let ((input (open-input-string (file->string filename))))
   (explode input)))

(define answer (part-one "day09-data.txt"))
(printf "Part One: The string length of the answer is: ~a~n" (string-length answer))

;
; --- Part Two ---
;
; Apparently, the file actually uses version two of the format.
;
; In version two, the only difference is that markers within decompressed data are
;  decompressed. This, the documentation explains, provides much more substantial
;  compression capabilities, allowing many-gigabyte files to be stored in only a
;  few kilobytes.
;
; For example:
;
; * (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
;
; * X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data from
;    the (8x2) marker is then further decompressed, thus triggering the (3x3) marker
;    twice for a total of six ABC sequences.
;
; * (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times.
;
; * (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.
;
; Unfortunately, the computer you brought probably doesn't have enough memory to actually
;  decompress the file; you'll have to come up with another way to get its decompressed length.
;
; What is the decompressed length of the file using this improved format? (Answer: 11,125,026,826)
;

(define (exploder in-str)
  (let loop ((string-input in-str))
   (printf "Entering exploder with string length: ~a, and paren count: ~a~n"
           (string-length string-input)
           (length (regexp-match* #rx"\\(" string-input)))
   (let ((input (open-input-string string-input)))
    (if (regexp-match #rx"\\(" string-input)
        (loop (explode input))
        (explode input)))))

;(define (part-two filename) (exploder (file->string filename)))
;(printf "Part Two: The string lenth of the answer is: ~a~n" (part-two "day09-data.txt")) ; This runs forever and then blows up. :-(

;
; Rethinking how to solve this problem...
;

(define (tally str)
  (let ((pieces (regexp-match* #rx"^(.*?)\\(([0-9]+)x([0-9]+)\\)(.*)$" str #:match-select values)))
    (if (null? pieces)
        (string-length str)
        (let ((mtch (car pieces)))
         (define-values (_ pre num1-str num2-str post) (apply values mtch))
         (define leng (string->number num1-str))
         (define mult (string->number num2-str))
         (+ (string-length pre)
            (if (regexp-match #rx"\\(" post)
                (+ (* mult (tally (substring post 0 leng))) (tally (substring post leng)))
                (+ (* mult (string-length post)))))))))

(check-equal? (tally "XYZ") 3)
(check-equal? (tally "(3x3)XYZ") 9)
(check-equal? (tally "X(8x2)(3x3)ABCY") 20)
(check-equal? (tally "(27x12)(20x12)(13x14)(7x10)(1x12)A") 241920)
(check-equal? (tally "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") 445)

(define (part-two filename) (tally (file->string filename))) ; -> 11125026826

