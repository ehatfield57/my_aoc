
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 10: The Stars Align ---
;
; It's no use; your navigation system simply isn't capable of
;  providing walking directions in the arctic circle, and
;  certainly not in 1018.
;
; The Elves suggest an alternative. In times like these, North
;  Pole rescue operations will arrange points of light in the sky
;  to guide missing Elves back to base. Unfortunately, the message
;  is easy to miss: the points move slowly enough that it takes
;  hours to align them, but have so much momentum that they only
;  stay aligned for a second. If you blink at the wrong time, it
;  might be hours before another message appears.
;
; You can see these points of light floating in the distance, and
;  record their position in the sky and their velocity, the relative
;  change in position per second (your puzzle input). The coordinates
;  are all given from your perspective; given enough time, those
;  positions and velocities will move the points into a cohesive message!
;
; Rather than wait, you decide to fast-forward the process and calculate
;  what the points will eventually spell.
;
; For example, suppose you note the following points:
;
; position=< 9,  1> velocity=< 0,  2>
; position=< 7,  0> velocity=<-1,  0>
; position=< 3, -2> velocity=<-1,  1>
; position=< 6, 10> velocity=<-2, -1>
; position=< 2, -4> velocity=< 2,  2>
; position=<-6, 10> velocity=< 2, -2>
; position=< 1,  8> velocity=< 1, -1>
; position=< 1,  7> velocity=< 1,  0>
; position=<-3, 11> velocity=< 1, -2>
; position=< 7,  6> velocity=<-1, -1>
; position=<-2,  3> velocity=< 1,  0>
; position=<-4,  3> velocity=< 2,  0>
; position=<10, -3> velocity=<-1,  1>
; position=< 5, 11> velocity=< 1, -2>
; position=< 4,  7> velocity=< 0, -1>
; position=< 8, -2> velocity=< 0,  1>
; position=<15,  0> velocity=<-2,  0>
; position=< 1,  6> velocity=< 1,  0>
; position=< 8,  9> velocity=< 0, -1>
; position=< 3,  3> velocity=<-1,  1>
; position=< 0,  5> velocity=< 0, -1>
; position=<-2,  2> velocity=< 2,  0>
; position=< 5, -2> velocity=< 1,  2>
; position=< 1,  4> velocity=< 2,  1>
; position=<-2,  7> velocity=< 2, -2>
; position=< 3,  6> velocity=<-1, -1>
; position=< 5,  0> velocity=< 1,  0>
; position=<-6,  0> velocity=< 2,  0>
; position=< 5,  9> velocity=< 1, -2>
; position=<14,  7> velocity=<-2,  0>
; position=<-3,  6> velocity=< 2, -1>
;
; Each line represents one point. Positions are given as <X, Y> pairs: X
;  represents how far left (negative) or right (positive) the point appears,
;  while Y represents how far up (negative) or down (positive) the point
;  appears.
;
; At 0 seconds, each point has the position given. Each second, each point's
;  velocity is added to its position. So, a point with velocity <1, -2> is
;  moving to the right, but is moving upward twice as quickly. If this point's
;  initial position were <3, 9>, after 3 seconds, its position would become <6, 3>.
;
; Over time, the points listed above would move like this:
;
; Initially:
; ........#.............
; ................#.....
; .........#.#..#.......
; ......................
; #..........#.#.......#
; ...............#......
; ....#.................
; ..#.#....#............
; .......#..............
; ......#...............
; ...#...#.#...#........
; ....#..#..#.........#.
; .......#..............
; ...........#..#.......
; #...........#.........
; ...#.......#..........
;
; After 1 second:
; ......................
; ......................
; ..........#....#......
; ........#.....#.......
; ..#.........#......#..
; ......................
; ......#...............
; ....##.........#......
; ......#.#.............
; .....##.##..#.........
; ........#.#...........
; ........#...#.....#...
; ..#...........#.......
; ....#.....#.#.........
; ......................
; ......................
;
; After 2 seconds:
; ......................
; ......................
; ......................
; ..............#.......
; ....#..#...####..#....
; ......................
; ........#....#........
; ......#.#.............
; .......#...#..........
; .......#..#..#.#......
; ....#....#.#..........
; .....#...#...##.#.....
; ........#.............
; ......................
; ......................
; ......................
;
; After 3 seconds:
; ......................
; ......................
; ......................
; ......................
; ......#...#..###......
; ......#...#...#.......
; ......#...#...#.......
; ......#####...#.......
; ......#...#...#.......
; ......#...#...#.......
; ......#...#...#.......
; ......#...#..###......
; ......................
; ......................
; ......................
; ......................
;
; After 4 seconds:
; ......................
; ......................
; ......................
; ............#.........
; ........##...#.#......
; ......#.....#..#......
; .....#..##.##.#.......
; .......##.#....#......
; ...........#....#.....
; ..............#.......
; ....#......#...#......
; .....#.....##.........
; ...............#......
; ...............#......
; ......................
; ......................
;
; After 3 seconds, the message appeared briefly: HI. Of course, your message
;  will be much longer and will take many more seconds to appear.
;
; What message will eventually appear in the sky? (Answer: PLBPGFRR)
;

; position=<-41933,  10711> velocity=< 4, -1>

(define (get-points filename)
  (let ((points (make-hash))
        (vectors (make-hash))
        (lines (file->lines filename))
        (idx 0))
    (for ((line lines))
         (define-values (x y acc-x acc-y) (apply values (map string->number (regexp-match* #rx"([0-9-]+)" line #:match-select car))))
         (hash-set! points idx (cons x y))
         (hash-set! vectors idx (cons acc-x acc-y))
         (set! idx (add1 idx)))
    (values points vectors)))

(define (draw-points points)
  (let ((coors (make-hash)))
    (define-values (min-x min-y max-x max-y) (min-maxes points))
    (for ((idx (hash-keys points)))
         (hash-set! coors (hash-ref points idx) idx))

    (for ((y (range min-y (add1 max-y))))
         (for ((x (range min-x (add1 max-x))))
              (if (hash-has-key? coors (cons x y))
                  (printf "#")
                  (printf ".")))
         (printf "~n"))
    (printf "~n")))

(define (tick points vectors)
  (let ((new-points (make-hash)))
   (for ((idx (hash-keys points)))
        (hash-set! new-points idx
                   (cons
                     (+ (car (hash-ref points idx))
                        (car (hash-ref vectors idx)))
                     (+ (cdr (hash-ref points idx))
                        (cdr (hash-ref vectors idx)))
                     )))
   new-points))

(define (min-maxes points)
  (let  ((min-x (apply min (map car (hash-values points))))
         (min-y (apply min (map cdr (hash-values points))))
         (max-x (apply max (map car (hash-values points))))
         (max-y (apply max (map cdr (hash-values points)))))
    (values min-x min-y max-x max-y)))

(define (compute-size points)
  (define-values (min-x min-y max-x max-y) (min-maxes points))
  (* (- max-x min-x) (- max-y min-y)))

(define (part-one filename)
  (let ((min-size 99999999999)
        (min-points #f))
    (define-values (points vectors) (get-points filename))
    (let loop ((sec 0))
     (define cur-size (compute-size points))

     (when (or (equal? min-points #f)
                (< cur-size min-size))
       (set! min-points points)
       (set! min-size cur-size))

     (when (and (not (equal? min-points #f))
                (> cur-size min-size))
       (printf "After ~a seconds:~n" (sub1 sec))
       (draw-points min-points)
       (error "Stopping now."))

     (set! points (tick points vectors))

     (loop (add1 sec)))))

;
; --- Part Two ---
;
; Good thing you didn't have to wait, because that would have taken a
;  long time - much longer than the 3 seconds in the example above.
;
; Impressed by your sub-hour communication capabilities, the Elves are
;  curious: exactly how many seconds would they have needed to wait for
;  that message to appear? (Answer: 10519 seconds)
;

