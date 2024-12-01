
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 14: Disk Defragmentation ---
;
; Suddenly, a scheduled job activates the system's disk defragmenter.
;  Were the situation different, you might sit and watch it for a while,
;  but today, you just don't have that kind of time. It's soaking up
;  valuable system resources that are needed elsewhere, and so the only
;  option is to help it finish its task as soon as possible.
;
; The disk in question consists of a 128x128 grid; each square of the
;  grid is either free or used. On this disk, the state of the grid is
;  tracked by the bits in a sequence of knot hashes.
;
; A total of 128 knot hashes are calculated, each corresponding to a
;  single row in the grid; each hash contains 128 bits which correspond
;  to individual grid squares. Each bit of a hash indicates whether that
;  square is free (0) or used (1).
;
; The hash inputs are a key string (your puzzle input), a dash, and a
;  number from 0 to 127 corresponding to the row. For example, if your
;  key string were flqrgnkx, then the first row would be given by the
;  bits of the knot hash of flqrgnkx-0, the second row from the bits
;  of the knot hash of flqrgnkx-1, and so on until the last row, flqrgnkx-127.
;
; The output of a knot hash is traditionally represented by 32 hexadecimal
;  digits; each of these digits correspond to 4 bits, for a total of 4 * 32
;  = 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent
;  binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110,
;  f becomes 1111, and so on; a hash that begins with a0c2017... in hexadecimal
;  would begin with 10100000110000100000000101110000... in binary.
;
; Continuing this process, the first 8 rows and columns for key flqrgnkx appear
;  as follows, using # to denote used squares, and . to denote free ones:
;
; ##.#.#..-->
; .#.#.#.#   
; ....#.#.   
; #.#.##.#   
; .##.#...   
; ##..#..#   
; .#...#..   
; ##.#.##.-->
; |      |   
; V      V   
;
; In this example, 8108 squares are used across the entire 128x128 grid.
;
; Given your actual key string, how many squares are used? (Answer: 8250)
;
; Your puzzle input is stpzcrnm.
;

;
; All of this is from Day 10, so we can produce knot hashes...
;
(define (string-circle str-len)
  (let* ((cur-pos 0)
         (skip-amt 0)
         (the-cir (for/vector ((i (range str-len))) i))
         (cir-idx (lambda (i) (modulo i str-len)))
         (idx-lst (lambda (len) (map (lambda (n) (cir-idx (+ n cur-pos))) (range len)))))
    (lambda (cmd . args)
      (case cmd
        ((input)
         (let* ((the-len (car args))
                (indexes (idx-lst the-len))
                (rev-idxs (reverse indexes))
                (temp (make-vector (length indexes))))
           (for ((i (range (length indexes))))
                (vector-set! temp i (vector-ref the-cir (list-ref rev-idxs i))))
           (for ((i (range (length indexes)))) (vector-set! the-cir (list-ref indexes i) (vector-ref temp i)))
           (set! cur-pos (cir-idx (+ the-len skip-amt cur-pos)))
           (set! skip-amt (add1 skip-amt)))
         (* (vector-ref the-cir 0) (vector-ref the-cir 1)))
        ((show) (format "the-cir: ~a, cur-pos: ~a, skip-amt: ~a" the-cir cur-pos skip-amt))
        ((list) the-cir)
        (else (error (format "Unknown cmd '~a' with args: ~a~n" cmd args)))))))

(define (string->ascii str)
  (map char->integer (string->list str)))

(define (required-append ascii-list)
  (append ascii-list '(17 31 73 47 23)))

(check-equal? (string->ascii "1,2,3") '(49 44 50 44 51))
(check-equal? (required-append (string->ascii "1,2,3")) '(49 44 50 44 51 17 31 73 47 23))

(define (sparse-hash size lengths)
  (let ((machine (string-circle size))
        (return 0)) 
    (for ((i (range 64)))
         (for ((num lengths))
              (machine 'input num)))
    (vector->list (machine 'list))))

(define (dense-hash numbers)
  (let ((current-list numbers)
        (output-list '()))
    (let loop ((first-16 (take current-list 16))
               (the-rest (drop current-list 16)))
      (set! output-list (cons (apply bitwise-xor first-16) output-list))
      (if (zero? (length the-rest))
          (reverse output-list)
          (loop (take the-rest 16) (drop the-rest 16))))))

(define (pad-left2 num)
  (let ((hex (number->string num 16)))
   (if (>= (string-length hex) 2)
       hex 
       (string-append "0" hex))))

(define (knot-hash str)
  (apply string-append
         (map pad-left2
              (dense-hash
                (sparse-hash 256 
                             (required-append
                               (string->ascii str)))))))

(check-equal? (knot-hash "") "a2582a3a0e66e6e86e3812dcb672a272")
(check-equal? (knot-hash "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd")
(check-equal? (knot-hash "1,2,3") "3efbe78a8d82f29979031a4aa0b16a9d")
(check-equal? (knot-hash "1,2,4") "63960835bcdc130f0b66d7ff4f6a5a8e")
;
; Done with code inclusion from Day 10 so we can have knot hash...
;

(define (pad-left width ch str)
  (if (>= (string-length str) width)
      str
      (pad-left width ch (string-append ch str))))

(check-equal? (pad-left 2 "0" "1") "01")
(check-equal? (pad-left 4 "0" "1") "0001")

(define (hex->binary hex-str)
  (if (string=? hex-str "")
      ""
      (let* ((str-list (string->list hex-str))
             (ch-str (string (car str-list)))
             (rest-str (list->string (cdr str-list)))
             (bin-str (pad-left 4 "0" (number->string (string->number ch-str 16) 2))))
        (string-append bin-str (hex->binary rest-str)))))

(check-equal? (hex->binary "a0c2017") "1010000011000010000000010111")

(define (bin-str->hash-str str)
  (string-replace (string-replace str "0" ".") "1" "#"))

(define (hash-knot key-str num)
  (bin-str->hash-str (hex->binary (knot-hash (format "~a-~a" key-str (number->string num))))))

(check-equal? (substring (hash-knot "flqrgnkx" 0) 0 8) "##.#.#..")
(check-equal? (substring (hash-knot "flqrgnkx" 1) 0 8) ".#.#.#.#")
(check-equal? (substring (hash-knot "flqrgnkx" 2) 0 8) "....#.#.")
(check-equal? (substring (hash-knot "flqrgnkx" 3) 0 8) "#.#.##.#")
(check-equal? (substring (hash-knot "flqrgnkx" 4) 0 8) ".##.#...")
(check-equal? (substring (hash-knot "flqrgnkx" 5) 0 8) "##..#..#")
(check-equal? (substring (hash-knot "flqrgnkx" 6) 0 8) ".#...#..")
(check-equal? (substring (hash-knot "flqrgnkx" 7) 0 8) "##.#.##.")

(define (hash-count str)
  (string-length (string-replace str "." "")))

(define (part-one key-str)
  (let ((total 0)
        (test "")
        (out (open-output-file "output.txt" #:exists 'replace)))
   (for ((n (range 128)))
        (set! test (hash-knot key-str n))
        (fprintf out "~a~n" test)
        (set! total (+ total (hash-count test))))
   total))

(define test-input "flqrgnkx")
(define puzzle-input "stpzcrnm")

(check-equal? (part-one test-input) 8108)
(check-equal? (part-one puzzle-input) 8250)

;
; --- Part Two ---
;
; Now, all the defragmenter needs to know is the number of regions. A
;  region is a group of used squares that are all adjacent, not including
;  diagonals. Every used square is in exactly one region: lone used squares
;  form their own isolated regions, while several adjacent squares all
;  count as a single region.
;
; In the example above, the following nine regions are visible, each marked
;  with a distinct digit:
;
; 11.2.3..-->
; .1.2.3.4   
; ....5.6.   
; 7.8.55.9   
; .88.5...   
; 88..5..8   
; .8...8..   
; 88.8.88.-->
; |      |   
; V      V   
;
; Of particular interest is the region marked 8; while it does not appear
;  contiguous in this small view, all of the squares marked 8 are connected
;  when considering the whole 128x128 grid. In total, in this example, 1242
;  regions are present.
;
; How many regions are present given your key string? (Answer: 1113)
;

(define (calc-near-coor array coor)
  (let* ((x (car coor))
         (y (cdr coor))
         (n (cons x (sub1 y)))
         (s (cons x (add1 y)))
         (e (cons (add1 x) y))
         (w (cons (sub1 x) y))
         (return '()))
    (when (hash-has-key? array n) (set! return (cons n return)))
    (when (hash-has-key? array s) (set! return (cons s return)))
    (when (hash-has-key? array e) (set! return (cons e return)))
    (when (hash-has-key? array w) (set! return (cons w return)))
    return))

(define (delete-group array coor)
  (hash-remove! array coor)
  (for ((near-coor (calc-near-coor array coor)))
       (when (hash-has-key? array near-coor)
         (delete-group array near-coor))))

(define (count-groups filename)
  (let ((lines (file->lines filename))
        (array (make-hash))
        (total 0) (x 0) (y 0))
; Fill array hash with locations of all hash marks
    (for ((line lines))
         (for ((ch (string->list line)))
              (when (char=? ch #\#)
                (hash-set! array (cons x y) #f))
              (set! x (add1 x)))
         (set! y (add1 y))
         (set! x 0))
; Count the number of groups in hash array
    (for ((coor (hash-keys array)))
         (when (hash-has-key? array coor)
             (set! total (add1 total))
             (delete-group array coor)))
  total))

(check-equal? (part-one test-input) 8108)
(check-equal? (count-groups "output.txt") 1242)

(check-equal? (part-one puzzle-input) 8250)
(check-equal? (count-groups "output.txt") 1113)

