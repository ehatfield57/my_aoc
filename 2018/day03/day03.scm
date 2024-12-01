
(require rackunit)
(require srfi/1)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 3: No Matter How You Slice It ---
;
; The Elves managed to locate the chimney-squeeze prototype fabric
;  for Santa's suit (thanks to someone who helpfully wrote its box
;  IDs on the wall of the warehouse in the middle of the night).
;  Unfortunately, anomalies are still affecting them - nobody can
;  even agree on how to cut the fabric.
;
; The whole piece of fabric they're working on is a very large square
;  - at least 1000 inches on each side.
;
; Each Elf has made a claim about which area of fabric would be ideal
;  for Santa's suit. All claims have an ID and consist of a single
;  rectangle with edges parallel to the edges of the fabric. Each
;  claim's rectangle is defined as follows:
;
; * The number of inches between the left edge of the fabric and the left edge of the rectangle.
; * The number of inches between the top edge of the fabric and the top edge of the rectangle.
; * The width of the rectangle in inches.
; * The height of the rectangle in inches.
;
; A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a
;  rectangle 3 inches from the left edge, 2 inches from the top edge,
;  5 inches wide, and 4 inches tall. Visually, it claims the square
;  inches of fabric represented by # (and ignores the square inches
;  of fabric represented by .) in the diagram below:
;
; ...........
; ...........
; ...#####...
; ...#####...
; ...#####...
; ...#####...
; ...........
; ...........
; ...........
;
; The problem is that many of the claims overlap, causing two or more
;  claims to cover part of the same areas. For example, consider the
;  following claims:
;
; #1 @ 1,3: 4x4
; #2 @ 3,1: 4x4
; #3 @ 5,5: 2x2
;
; Visually, these claim the following areas:
;
; ........
; ...2222.
; ...2222.
; .11XX22.
; .11XX22.
; .111133.
; .111133.
; ........
;
; The four square inches marked with X are claimed by both 1 and 2. (Claim
;  3, while adjacent to the others, does not overlap either of them.)
;
; If the Elves all proceed with their own plans, none of them will have
;  enough fabric. How many square inches of fabric are within two or
;  more claims? (Answer: 116489)
;

(define (overlap fabric)
  (let ((result 0))
   (for ((key (hash-keys fabric)))
        (define elf-count (length (hash-ref fabric key)))
        (when (> elf-count 1)
          (set! result (add1 result)) ; -> 112275 and 112276 (too low)
;          (set! result (+ (sub1 elf-count) result)) ; -> 143748 (too high)
          ))
   result))

(define (get-claims filename)
  (let ((lines (file->lines filename))
        (fabric (make-hash)))
    (for ((line lines))
         (define-values (id left top width height) (apply values (map string->number (regexp-match* "([0-9]+)" line))))
         (for ((h (range height)))
              (for ((w (range width)))
                   (define key (cons (+ w left) (+ h top)))
                   (if (hash-has-key? fabric key)
                       (hash-set! fabric key (cons id (hash-ref fabric key)))
                       (hash-set! fabric key (list id))))))
    fabric))

(define (part-one filename) (overlap (get-claims filename)))

(define (next-char ch) (integer->char (add1 (char->integer ch))))

(define (draw-fabric fabric width height)
  (let ((legend (make-hash))
        (cur-ch #\0))
    (for ((h (range height)))
         (for ((w (range width)))
              (define key (cons w h))
              (cond
                ((not (hash-has-key? fabric key)) (printf "."))
                ((> (length (hash-ref fabric key)) 1) (printf "X"))
                (else
                  (define id (car (hash-ref fabric key)))
                  (when (not (hash-has-key? legend id))
                    (hash-set! legend id cur-ch)
                    (set! cur-ch (next-char cur-ch)))
                  (printf "~a" (hash-ref legend id))

                  )))
         (printf "~n"))
    (printf "~nLegend: ~a~n" legend)))

(check-equal? (part-one "test-data.txt") 4)
(draw-fabric (get-claims "test-data.txt") 8 8)

(check-equal? (part-one "day03-data.txt") 116489)

;
; --- Part Two ---
;
; Amidst the chaos, you notice that exactly one claim doesn't overlap
;  by even a single square inch of fabric with any other claim. If you
;  can somehow draw attention to it, maybe the Elves will be able to
;  make Santa's suit after all!
;
; For example, in the claims above, only claim 3 is intact after all
;  claims are made.
;
; What is the ID of the only claim that doesn't overlap? (Answer: 1260)
;

(define (part-two filename)
  (let* ((claims (get-claims filename))
         (all-ids (remove-duplicates (flatten (hash-values claims))))
         (overlap-ids (remove-duplicates (flatten (filter
                                                    (lambda (l) (> (length l) 1))
                                                    (hash-values claims))))))
    (car (lset-difference = all-ids overlap-ids))))

(check-equal? (part-two "test-data.txt") 3)
(check-equal? (part-two "day03-data.txt") 1260)

