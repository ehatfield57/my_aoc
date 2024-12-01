
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 18: Settlers of The North Pole ---
;
; On the outskirts of the North Pole base construction project, many
;  Elves are collecting lumber.
;
; The lumber collection area is 50 acres by 50 acres; each acre can be
;  either open ground (.), trees (|), or a lumberyard (#). You take a
;  scan of the area (your puzzle input).
;
; Strange magic is at work here: each minute, the landscape looks entirely
;  different. In exactly one minute, an open acre can fill with trees, a
;  wooded acre can be converted to a lumberyard, or a lumberyard can be
;  cleared to open ground (the lumber having been sent to other projects).
;
; The change to each acre is based entirely on the contents of that acre as
;  well as the number of open, wooded, or lumberyard acres adjacent to it
;  at the start of each minute. Here, "adjacent" means any of the eight
;  acres surrounding that acre. (Acres on the edges of the lumber collection
;  area might have fewer than eight adjacent acres; the missing acres aren't
;  counted.)
;
; In particular:
;
; * An open acre will become filled with trees if three or more adjacent
;    acres contained trees. Otherwise, nothing happens.
; * An acre filled with trees will become a lumberyard if three or more
;    adjacent acres were lumberyards. Otherwise, nothing happens.
; * An acre containing a lumberyard will remain a lumberyard if it was
;    adjacent to at least one other lumberyard and at least one acre
;    containing trees. Otherwise, it becomes open.
;
; These changes happen across all acres simultaneously, each of them using
;  the state of all acres at the beginning of the minute and changing to
;  their new form by the end of that same minute. Changes that happen during
;  the minute don't affect each other.
;
; For example, suppose the lumber collection area is instead only 10 by 10
;  acres with this initial configuration:
;
; Initial state:
; .#.#...|#.
; .....#|##|
; .|..|...#.
; ..|#.....#
; #.#|||#|#|
; ...#.||...
; .|....|...
; ||...#|.#|
; |.||||..|.
; ...#.|..|.
;
; After 1 minute:
; .......##.
; ......|###
; .|..|...#.
; ..|#||...#
; ..##||.|#|
; ...#||||..
; ||...|||..
; |||||.||.|
; ||||||||||
; ....||..|.
;
; After 2 minutes:
; .......#..
; ......|#..
; .|.|||....
; ..##|||..#
; ..###|||#|
; ...#|||||.
; |||||||||.
; ||||||||||
; ||||||||||
; .|||||||||
;
; After 3 minutes:
; .......#..
; ....|||#..
; .|.||||...
; ..###|||.#
; ...##|||#|
; .||##|||||
; ||||||||||
; ||||||||||
; ||||||||||
; ||||||||||
;
; After 4 minutes:
; .....|.#..
; ...||||#..
; .|.#||||..
; ..###||||#
; ...###||#|
; |||##|||||
; ||||||||||
; ||||||||||
; ||||||||||
; ||||||||||
;
; After 5 minutes:
; ....|||#..
; ...||||#..
; .|.##||||.
; ..####|||#
; .|.###||#|
; |||###||||
; ||||||||||
; ||||||||||
; ||||||||||
; ||||||||||
;
; After 6 minutes:
; ...||||#..
; ...||||#..
; .|.###|||.
; ..#.##|||#
; |||#.##|#|
; |||###||||
; ||||#|||||
; ||||||||||
; ||||||||||
; ||||||||||
;
; After 7 minutes:
; ...||||#..
; ..||#|##..
; .|.####||.
; ||#..##||#
; ||##.##|#|
; |||####|||
; |||###||||
; ||||||||||
; ||||||||||
; ||||||||||
;
; After 8 minutes:
; ..||||##..
; ..|#####..
; |||#####|.
; ||#...##|#
; ||##..###|
; ||##.###||
; |||####|||
; ||||#|||||
; ||||||||||
; ||||||||||
;
; After 9 minutes:
; ..||###...
; .||#####..
; ||##...##.
; ||#....###
; |##....##|
; ||##..###|
; ||######||
; |||###||||
; ||||||||||
; ||||||||||
;
; After 10 minutes:
; .||##.....
; ||###.....
; ||##......
; |##.....##
; |##.....##
; |##....##|
; ||##.####|
; ||#####|||
; ||||#|||||
; ||||||||||
;
; After 10 minutes, there are 37 wooded acres and 31 lumberyards. Multiplying
;  the number of wooded acres by the number of lumberyards gives the total
;  resource value after ten minutes: 37 * 31 = 1147.
;
; What will the total resource value of the lumber collection area be after
;  10 minutes? (Answer: 536370)
;

(define (scan-area filename)
  (let ((area (make-hash))
        (input (open-input-file filename))
        (x 0) (y 0))
    (let loop ((ch (read-char input)))
     (case ch
       ((#\. #\# #\|) (hash-set! area (cons x y) ch))
       ((#\newline)
        (set! y (add1 y))
        (set! x -1)))
     (set! x (add1 x))
     (set! ch (read-char input))
     (when (not (eof-object? ch)) (loop ch)))
    (close-input-port input)
    area))

(define (nearby-coors coor)
  (let ((x (car coor))
        (y (cdr coor)))
   (list (cons (sub1 x) (sub1 y))
         (cons       x  (sub1 y))
         (cons (add1 x) (sub1 y))
         (cons (sub1 x)       y)
         (cons (add1 x)       y)
         (cons (sub1 x) (add1 y))
         (cons       x  (add1 y))
         (cons (add1 x) (add1 y)))))

(define (neighbors area coor)
  (let ((tally (make-hash))
        (nearby (nearby-coors coor)))
    (for ((test-coor nearby))
         (when (hash-has-key? area test-coor)
           (define ch (hash-ref area test-coor))
           (if (hash-has-key? tally ch)
               (hash-set! tally ch (add1 (hash-ref tally ch)))
               (hash-set! tally ch 1))))
    tally))

(define (new-acre acre-ch neighbor-tally)
  (case acre-ch
    ((#\.) (if (and (hash-has-key? neighbor-tally #\|) (>= (hash-ref neighbor-tally #\|) 3)) #\| #\.))
    ((#\#) (if (and (and (hash-has-key? neighbor-tally #\#) (>= (hash-ref neighbor-tally #\#) 1))
                    (and (hash-has-key? neighbor-tally #\|) (>= (hash-ref neighbor-tally #\|) 1))) #\# #\.))
    ((#\|) (if (and (hash-has-key? neighbor-tally #\#) (>= (hash-ref neighbor-tally #\#) 3)) #\# #\|))))

(define (one-min area)
  (let ((new-area (make-hash)))
   (for ((coor (hash-keys area)))
        (hash-set! new-area coor (new-acre (hash-ref area coor) (neighbors area coor))))
   new-area))

(define (draw-area area)
  (let* ((coors (hash-keys area))
         (max-x (add1 (apply max (map car coors))))
         (max-y (add1 (apply max (map cdr coors)))))
    (for ((y (range max-y)))
         (for ((x (range max-x)))
              (printf "~a" (string (hash-ref area (cons x y)))))
         (printf "~n"))
    (printf "~n")))

(define (tally-area area)
  (let ((tally (make-hash)))
   (for ((coor (hash-keys area)))
        (define ch (hash-ref area coor))
        (if (hash-has-key? tally ch)
            (hash-set! tally ch (add1 (hash-ref tally ch)))
            (hash-set! tally ch 1)))
   tally))

(define (compute-answer tally)
   (* (hash-ref tally #\|) (hash-ref tally #\#)))

(define (part-one filename (minutes 10))
  (let ((area (scan-area filename)))
   (for ((minute (range minutes)))
        (printf "After ~a minutes:~n" minute)
        (draw-area area)
        (set! area (one-min area)))
   (compute-answer (tally-area area))))

; (check-equal? (part-one "test-data.txt") 1147)
; (check-equal? (part-one "day18-data.txt") 536370)

;
; --- Part Two ---
;
; This important natural resource will need to last for at least thousands of years.
;  Are the Elves collecting this lumber sustainably?
;
; What will the total resource value of the lumber collection area be after
;  1000000000 minutes? (Answer: 190512)
;

(define (part-two filename (every 10))
  (let ((area (scan-area filename))
        (answer 0))
    (let loop ((minute 0))
     (set! answer (compute-answer (tally-area area)))
     (when (zero? (modulo minute every)) (printf "~a - ~a~n" minute answer))
     (set! area (one-min area))
     (when (< minute 1000000000)
       (loop (add1 minute))))))

; (part-two "day18-data.txt" 100)

