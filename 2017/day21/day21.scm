
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 21: Fractal Art ---
;
; You find a program trying to generate some art. It uses a strange process
;  that involves repeatedly enhancing the detail of an image through a set
;  of rules.
;
; The image consists of a two-dimensional square grid of pixels that are either
;  on (#) or off (.). The program always begins with this pattern:
;
; .#.
; ..#
; ###
;
; Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to
;  have a size of 3.
;
; Then, the program repeats the following process:
;
; If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and
;  convert each 2x2 square into a 3x3 square by following the corresponding
;  enhancement rule.
;
; Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3 squares,
;  and convert each 3x3 square into a 4x4 square by following the corresponding
;  enhancement rule.
;
; Because each square of pixels is replaced by a larger one, the image gains pixels
;  and so its size increases.
;
; The artist's book of enhancement rules is nearby (your puzzle input); however, it
;  seems to be missing rules. The artist explains that sometimes, one must rotate or
;  flip the input pattern to find a match. (Never rotate or flip the output pattern,
;  though.) Each pattern is written concisely: rows are listed as single units, ordered
;  top-down, and separated by slashes. For example, the following rules correspond to
;  the adjacent patterns:
;
; ../.#  =  ..
;           .#
;
;                 .#.
; .#./..#/###  =  ..#
;                 ###
;
;                         #..#
; #..#/..../#..#/.##.  =  ....
;                         #..#
;                         .##.
;
; When searching for a rule to use, rotate and flip the pattern as necessary. For example,
;  all of the following patterns match the same rule:
;
; .#.   .#.   #..   ###
; ..#   #..   #.#   ..#
; ###   ###   ##.   .#.
;
; Suppose the book contained the following two rules:
;
; ../.# => ##./#../...
; .#./..#/### => #..#/..../..../#..#
;
; As before, the program begins with this pattern:
;
; .#.
; ..#
; ###
;
; The size of the grid (3) is not divisible by 2, but it is divisible by 3. It divides evenly
;  into a single square; the square matches the second rule, which produces:
;
; #..#
; ....
; ....
; #..#
;
; The size of this enhanced grid (4) is evenly divisible by 2, so that rule is used. It divides
;  evenly into four squares:
;
; #.|.#
; ..|..
; --+--
; ..|..
; #.|.#
;
; Each of these squares matches the same rule (../.# => ##./#../...), three of which require
;  some flipping and rotation to line up with the rule. The output for the rule is the same
;  in all four cases:
;
; ##.|##.
; #..|#..
; ...|...
; ---+---
; ##.|##.
; #..|#..
; ...|...
;
; Finally, the squares are joined into a new grid:
;
; ##.##.
; #..#..
; ......
; ##.##.
; #..#..
; ......
;
; Thus, after 2 iterations, the grid contains 12 pixels that are on.
;
; How many pixels stay on after 5 iterations?  (Answer: ?)
;

(define start-pattern ".#./..#/###")

(define (size key)
  (index-of (string->list key) #\/))

(define (transform key code)
  (let* ((vec (list->vector (string->list key)))
         (vecx (lambda (x) (vector-ref vec x)))
         (indexes (map (lambda (s) (string->number s 16)) (map string (string->list code)))))
    (list->string (reverse (foldl (lambda (i acc) (cons (vecx i) acc)) '() indexes)))))

; Rotation codes strings:
(define rotate2-code "30241") ; '12/34' -> '31/42'
;                                01234      30241
(define rotate3-code "401385279a6") ; '123/456/789' -> '412/753/896'
;                                      0123456789a  ->  401385279a6
(define flip2-vert-code "34201") ; '12/34' -> '34/12' -> '21/43'
(define flip2-horz-code "10243") ;  01234      34201  ->  10243

(define flip3-vert-code "89a34567012") ; '123/456/789' -> '789/456/123' -> '321/654/987'
(define flip3-horz-code "21036547a98") ;  0123456789a  ->  89a34567012  --> 21036547a98

(define (rotate2 key) (transform key rotate2-code))
(define (rotate3 key) (transform key rotate3-code))
(define (flip2-vert key) (transform key flip2-vert-code))
(define (flip2-horz key) (transform key flip2-horz-code))
(define (flip3-vert key) (transform key flip3-vert-code))
(define (flip3-horz key) (transform key flip3-horz-code))

(define (rotate key)
  (if (zero? (modulo (size key) 2))
      (rotate2 key)
      (rotate3 key)))

(define (flip-vert key)
  (if (zero? (modulo (size key) 2))
      (flip2-vert key)
      (flip3-vert key)))

(define (flip-horz key)
  (if (zero? (modulo (size key) 2))
      (flip2-horz key)
      (flip3-horz key)))

(define (load-rules filename)
  (let ((lines (file->lines filename))
        (rules (make-hash)))
    (for ((line lines))
         (define-values (key val) (apply values (car (regexp-match* #rx"^(.*?) => (.*)" line #:match-select cdr))))
         (hash-set! rules key val)
         (hash-set! rules (rotate key) val)
         (hash-set! rules (rotate (rotate key)) val)
         (hash-set! rules (rotate (rotate (rotate key))) val)
         (hash-set! rules (flip-vert key) val)
         (hash-set! rules (rotate (flip-vert key)) val)
         (hash-set! rules (rotate (rotate (flip-vert key))) val)
         (hash-set! rules (rotate (rotate (rotate (flip-vert key)))) val)
         (hash-set! rules (flip-horz key) val)
         (hash-set! rules (rotate (flip-horz key)) val)
         (hash-set! rules (rotate (rotate (flip-horz key))) val)
         (hash-set! rules (rotate (rotate (rotate (flip-horz key)))) val))
    rules))

(define (step rules pattern)
  (cond
    ((list? pattern)
     (cond
       ((empty? pattern) '())
       (else (cons (step rules (car pattern)) (step rules (cdr pattern))))))
    ((string? pattern)
     (let ((p-size (size pattern)))
       (case p-size
         ((2 3) (hash-ref rules pattern))
         (else
           (step rules (splitter pattern))))))))

(define (splitter pat-str)
  (let* ((p-size (size pat-str))
         (half (if (zero? (modulo p-size 2)) 2 3))
         (p-half (/ p-size half))
         (raw-pat (string-replace pat-str "/" "" #:all? #t))
         (topleft (list 0 p-half (* p-size p-half) (+ (* p-half p-size) p-half)))
         (p-spans (map (lambda (x) (* p-size x)) (range p-half)))
         (chop   (lambda (n) (substring raw-pat n (+ n p-half))))
         (return '()))
    (for ((tl topleft))
         (let ((quarter '()))
          (for ((i p-spans))
               (set! quarter (cons (chop (+ i tl)) quarter)))
          (set! return (cons (string-join (reverse quarter) "/") return)) ))
    (reverse return)))

(define (count-on-pixels pattern)
  (cond
    ((list? pattern)
     (if (null? pattern)
         0
         (+ (count-on-pixels (car pattern)) (count-on-pixels (cdr pattern)))))
    ((string? pattern)
     (foldl (lambda (c a) (if (char=? c #\#) (add1 a) a)) 0 (string->list pattern)))))

(define (enhance filename iterations)
  (let* ((rules (load-rules filename))
         (pattern start-pattern))
    (let loop ((i 0))
     (printf "~a - ~a - ~a~n" i (count-on-pixels pattern) pattern)
     (set! pattern (step rules pattern))
     (when (< i iterations) (loop (add1 i))))))

; My code works, but comes up with the wrong answer. :-(  So I used
; reddit and someones python solution to get: 176 for part one.

;
; --- Part Two ---
;
; How many pixels stay on after 18 iterations? (Answer: 2368161)
;

; Again I had to use the reddit python code to get the correct answer.

