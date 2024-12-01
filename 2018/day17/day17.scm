(require rackunit)
(require racket/gui)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 17: Reservoir Research ---
;
; You arrive in the year 18. If it weren't for the coat you got in 1018,
;  you would be very cold: the North Pole base hasn't even been constructed.
;
; Rather, it hasn't been constructed yet. The Elves are making a little progress,
;  but there's not a lot of liquid water in this climate, so they're getting very
;  dehydrated. Maybe there's more underground?
;
; You scan a two-dimensional vertical slice of the ground nearby and discover that
;  it is mostly sand with veins of clay. The scan only provides data with a granularity
;  of square meters, but it should be good enough to determine how much water is trapped
;  there. In the scan, x represents the distance to the right, and y represents the
;  distance down. There is also a spring of water near the surface at x=500, y=0. The
;  scan identifies which square meters are clay (your puzzle input).
;
; For example, suppose your scan shows the following veins of clay:
;
; x=495, y=2..7
; y=7, x=495..501
; x=501, y=3..7
; x=498, y=2..4
; x=506, y=1..2
; x=498, y=10..13
; x=504, y=10..13
; y=13, x=498..504
;
; Rendering clay as #, sand as ., and the water spring as +, and with x increasing to
;  the right and y increasing downward, this becomes:
;
;    44444455555555
;    99999900000000
;    45678901234567
;  0 ......+.......
;  1 ............#.
;  2 .#..#.......#.
;  3 .#..#..#......
;  4 .#..#..#......
;  5 .#.....#......
;  6 .#.....#......
;  7 .#######......
;  8 ..............
;  9 ..............
; 10 ....#.....#...
; 11 ....#.....#...
; 12 ....#.....#...
; 13 ....#######...
;
; The spring of water will produce water forever. Water can move through sand, but is
;  blocked by clay. Water always moves down when possible, and spreads to the left and
;  right otherwise, filling space that has clay on both sides and falling out otherwise.
;
; For example, if five squares of water are created, they will flow downward until they
;  reach the clay and settle there. Water that has come to rest is shown here as ~, while
;  sand through which water has passed (but which is now dry again) is shown as |:
;
; ......+.......
; ......|.....#.
; .#..#.|.....#.
; .#..#.|#......
; .#..#.|#......
; .#....|#......
; .#~~~~~#......
; .#######......
; ..............
; ..............
; ....#.....#...
; ....#.....#...
; ....#.....#...
; ....#######...
;
; Two squares of water can't occupy the same location. If another five squares of water are
;  created, they will settle on the first five, filling the clay reservoir a little more:
;
; ......+.......
; ......|.....#.
; .#..#.|.....#.
; .#..#.|#......
; .#..#.|#......
; .#~~~~~#......
; .#~~~~~#......
; .#######......
; ..............
; ..............
; ....#.....#...
; ....#.....#...
; ....#.....#...
; ....#######...
;
; Water pressure does not apply in this scenario. If another four squares of water are created,
;  they will stay on the right side of the barrier, and no water will reach the left side:
;
; ......+.......
; ......|.....#.
; .#..#.|.....#.
; .#..#~~#......
; .#..#~~#......
; .#~~~~~#......
; .#~~~~~#......
; .#######......
; ..............
; ..............
; ....#.....#...
; ....#.....#...
; ....#.....#...
; ....#######...
;
; At this point, the top reservoir overflows. While water can reach the tiles above the surface of
;  the water, it cannot settle there, and so the next five squares of water settle like this:
;
; ......+.......
; ......|.....#.
; .#..#||||...#.
; .#..#~~#|.....
; .#..#~~#|.....
; .#~~~~~#|.....
; .#~~~~~#|.....
; .#######|.....
; ........|.....
; ........|.....
; ....#...|.#...
; ....#...|.#...
; ....#~~~~~#...
; ....#######...
;
; Note especially the leftmost |: the new squares of water can reach this tile, but cannot stop there.
;  Instead, eventually, they all fall to the right and settle in the reservoir below.
;
; After 10 more squares of water, the bottom reservoir is also full:
;
; ......+.......
; ......|.....#.
; .#..#||||...#.
; .#..#~~#|.....
; .#..#~~#|.....
; .#~~~~~#|.....
; .#~~~~~#|.....
; .#######|.....
; ........|.....
; ........|.....
; ....#~~~~~#...
; ....#~~~~~#...
; ....#~~~~~#...
; ....#######...
;
; Finally, while there is nowhere left for the water to settle, it can reach a few more tiles
;  before overflowing beyond the bottom of the scanned data:
;
; ......+.......    (line not counted: above minimum y value)
; ......|.....#.
; .#..#||||...#.
; .#..#~~#|.....
; .#..#~~#|.....
; .#~~~~~#|.....
; .#~~~~~#|.....
; .#######|.....
; ........|.....
; ...|||||||||..
; ...|#~~~~~#|..
; ...|#~~~~~#|..
; ...|#~~~~~#|..
; ...|#######|..
; ...|.......|..    (line not counted: below maximum y value)
; ...|.......|..    (line not counted: below maximum y value)
; ...|.......|..    (line not counted: below maximum y value)
;
; How many tiles can be reached by the water? To prevent counting forever, ignore tiles with
;  a y coordinate smaller than the smallest y coordinate in your scan data or larger than
;  the largest one. Any x coordinate is valid. In this example, the lowest y coordinate given
;  is 1, and the highest is 13, causing the water spring (in row 0) and the water falling off
;  the bottom of the render (in rows 14 through infinity) to be ignored.
;
; So, in the example above, counting both water at rest (~) and other sand tiles the water can
;  hypothetically reach (|), the total number of tiles the water can reach is 57.
;
; How many tiles can the water reach within the range of y values in your scan? (Answer: 33362)
;

(define spring (cons 500 0))
(define range-ok? #f)
(define get-xy #f)

(define (load-scan filename)
  (let ((ground (make-hash))
        (lines (file->lines filename)))
    (hash-set! ground spring "+")

    (for ((line lines))
         (define-values
           (coor num num-from num-to)
           (apply values (car
                           (regexp-match* #px"^([xy])=([0-9]+).*?([0-9]+).*?([0-9]+)" line #:match-select cdr))))

         (set! num (string->number num))
         (for ((other (range (string->number num-from) (add1 (string->number num-to)))))
              (define coors (if (string=? "x" coor) (cons num other) (cons other num)))
              (hash-set! ground coors "#")))
    (set! range-ok? (create-range-check ground))
    (set! get-xy (create-get-xy ground))
    ground))

(define (draw-scan ground (coor #f))
  (let ((min-x (apply min (map car (hash-keys ground))))
        (min-y (apply min (map cdr (hash-keys ground))))
        (max-x (apply max (map car (hash-keys ground))))
        (max-y (apply max (map cdr (hash-keys ground)))))
    (for ((y (range min-y (add1 max-y))))
         (for ((x (range (sub1 min-x) (+ 2 max-x))))
              (if (and coor (equal? coor (cons x y)))
                  (printf ",")
                  (if (hash-has-key? ground (cons x y))
                      (printf (hash-ref ground (cons x y)))
                      (printf "."))))
         (printf "~n"))
    (printf "~n")))

(define (create-range-check ground)
  (let ((min-x (apply min (map car (hash-keys ground))))
        (max-x (apply max (map car (hash-keys ground))))
        (max-y (apply max (map cdr (hash-keys ground)))))
    (lambda (coor)
      (let ((x (car coor)) (y (cdr coor)))
       (and (<= (add1 y) max-y)      ; Not off bottom
            (>= (sub1 x) min-x)      ; Not off left
            (<= (add1 x) max-x)))))) ; Not off right

(define (create-get-xy ground)
  (lambda (coor)
    (cond
      ((not (hash-has-key? ground coor)) ".")
      (else (hash-ref ground coor)))))

(define (generic-fill-dir ground coor dir-fn)
  (let* ((x (car coor))
         (y (cdr coor))
         (ch-beside   (get-xy (cons (dir-fn x) y)))
         (ch-downdiag (get-xy (cons (dir-fn x) (add1 y)))))
    (cond
      ((and (string=? "#" ch-beside) (string=? "." (get-xy coor)))
       (hash-set! ground coor "-")
       "-")
      ((and (string=? "." ch-beside) (string=? "." ch-downdiag) (string=? "." (get-xy coor)))
       (hash-set! ground coor "|")
       "|")
      (else
        (let ((ch-return (generic-fill-dir ground (cons (dir-fn x) y) dir-fn)))
         (hash-set! ground coor ch-return)
         ch-return)))))

(define (create-get-ch coor)
  (let ((x (car coor))
        (y (cdr coor)))
    (lambda (cmd)
      (case cmd
        ((cur c) (get-xy coor))
        ((n)  (get-xy (cons       x  (sub1 y))))
        ((s)  (get-xy (cons       x  (add1 y))))
        ((e)  (get-xy (cons (add1 x)       y)))
        ((w)  (get-xy (cons (sub1 x)       y)))
        ((se) (get-xy (cons (add1 x) (add1 y))))
        ((sw) (get-xy (cons (sub1 x) (add1 y))))
        ((ne) (get-xy (cons (add1 x) (sub1 y))))
        ((nw) (get-xy (cons (sub1 x) (sub1 y))))))))

(define (make-water ground coor)
  (let ((ch (create-get-ch coor)))
   (when (or (string=? (ch 'c) ".") (string=? (ch 'c) "+") (string=? (ch 'c) "|"))
     (when (string=? (ch 'c) ".") (hash-set! ground coor "|")))))

(define (drip ground coor)
  (draw-scan ground coor)
  (let ((x (car coor))
        (y (cdr coor))
        (ch (create-get-ch coor)))
    (when (range-ok? coor)
      (make-water ground coor)

      (case (ch 's)
        ((".")
         (drip ground (cons x (add1 y))))

        (("|")
         (drip ground (cons x (add1 y))))

        (("#" "-")
         (hash-set! ground coor "-")
         (when (string=? (ch 'w) ".") (generic-fill-dir ground (cons (sub1 x) y) sub1))
         (when (string=? (ch 'e) ".") (generic-fill-dir ground (cons (add1 x) y) add1)))

        (else (format "Not sure:'~a'~n" (ch 's)))))))

(define (fill-above ground coor)
  (let ((ch (create-get-ch coor))
        (x (car coor))
        (y (cdr coor)))
    (printf "Hi Edward A, in fill-above with coor:~a, character:'~a'~n" coor (ch 'cur))
    (case (ch 'cur)
      (("#")
       (printf "Hi Edward B~n")
       ; Find left edge (w & nw == #)
       ; Find right edge (e & ne == #)
       ; If both edges, fill with '-' and recurse (sub1 y)
       ; If neither edges, fill with '|' and call 'fill' from ends
       ; Else fill with '-' and '|' from coor
       )
      (("-" "|")
       (printf "Hi Edward C~n")
       ; Find left edge (nw == #)
       ; Find right edge (ne == #)
       ; If both edges, fill with '-' and recurse (sub1 y)
       ; If neither edges, fill with '|' and call 'fill' from ends
       ; Else fill with '-' and '|' from coor
       ))
    ))

(define (fill ground start-coor)
  (draw-scan ground start-coor)
  (let loop ((coor start-coor))
   (let ((ch (create-get-ch coor))
         (x (car coor))
         (y (cdr coor)))
     (when (range-ok? coor)
       (case (ch 'cur)
         (("+")
          (loop (cons x (add1 y))))
         ((".")
          (hash-set! ground coor "|")
          (loop (cons x (add1 y))))
         (("#" "-" "|")
          (fill-above ground coor)))))))

(define (tally ground)
  (let ((types (make-hash))
        (total 0))
    (for ((coor (hash-keys ground)))
         (define type-key (hash-ref ground coor))
         (if (hash-has-key? types type-key)
             (hash-set! types type-key (add1 (hash-ref types type-key)))
             (hash-set! types type-key 1)))
    (when (hash-has-key? types "-") (set! total (+ total (hash-ref types "-"))))
    (when (hash-has-key? types "|") (set! total (+ total (hash-ref types "|"))))
    total))

(define (draw-ground ground)
  (let* ((points (hash-keys ground))
         (min-x (apply min (map car points)))
         (max-x (apply max (map car points)))
         (min-y (apply min (map cdr points)))
         (max-y (apply max (map cdr points)))
         (ground-width (max 50 (- max-x min-x)))
         (ground-height (max 50 (- max-y min-y))))

    (define my-frame (new frame% (label "Reservoir") (width ground-width) (height ground-height)))

    (define my-canvas (new canvas%
                           (parent my-frame)
                           (paint-callback
                             (lambda (canvas dc)
                               (let ((black-pen (new pen% (color "black")))
                                     (blue-pen (new pen% (color "blue"))))
                                 (for ((point points))
                                      (let ((x (car point))
                                            (y (cdr point)))
                                        (send dc set-pen (if (string=? "#" (hash-ref ground point)) black-pen blue-pen))
                                        (send dc draw-point (add1 (- x min-x)) (add1 (- y min-y))))))))))
    (send my-frame show #t)))

(define (part-one filename)
  (let ((ground (load-scan filename))
        (prev-tally 0)
        (cur-tally 0))
    (let loop ((cnt 0))
     (printf "~a " cnt)
     (fill ground spring)
     (set! cur-tally (tally ground))
     (when (or (zero? prev-tally) (not (= cur-tally prev-tally)))
       (set! prev-tally cur-tally)
       (loop (add1 cnt))))
    ;    (draw-ground ground)
    (tally ground)))

(part-one "test-data.txt")
; (part-one "day17-data.txt")

;
; --- Part Two ---
;
; After a very long time, the water spring will run dry. How much water will
;  be retained?
;
; In the example above, water that won't eventually drain out is shown as ~,
;  a total of 29 tiles.
;
; How many water tiles are left after the water spring stops producing water
;  and all remaining water not at rest has drained? (Answer: 27801)
;

