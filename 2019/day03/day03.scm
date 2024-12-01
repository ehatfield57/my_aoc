
(require rackunit)
(require racket/trace)

;
; Advent of Code - 2019
;
; --- Day 3: Crossed Wires ---
;
; The gravity assist was successful, and you're well on your way to the
;  Venus refuelling station. During the rush back on Earth, the fuel
;  management system wasn't completely installed, so that's next on the
;  priority list.
;
; Opening the front panel reveals a jumble of wires. Specifically, two wires
;  are connected to a central port and extend outward on a grid. You trace
;  the path each wire takes as it leaves the central port, one wire per line
;  of text (your puzzle input).
;
; The wires twist and turn, but the two wires occasionally cross paths. To
;  fix the circuit, you need to find the intersection point closest to the
;  central port. Because the wires are on a grid, use the Manhattan distance
;  for this measurement. While the wires do technically cross right at the
;  central port where they both start, this point does not count, nor does
;  a wire count as crossing with itself.
;
; For example, if the first wire's path is R8,U5,L5,D3, then starting from
;  the central port (o), it goes right 8, up 5, left 5, and finally down 3:
;
; ...........
; ...........
; ...........
; ....+----+.
; ....|....|.
; ....|....|.
; ....|....|.
; .........|.
; .o-------+.
; ...........
;
; Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4,
;  and left 4:
;
; ...........
; .+-----+...
; .|.....|...
; .|..+--X-+.
; .|..|..|.|.
; .|.-X--+.|.
; .|..|....|.
; .|.......|.
; .o-------+.
; ...........
;
; These wires cross at two locations (marked X), but the lower-left one is closer
;  to the central port: its distance is 3 + 3 = 6.
;
; Here are a few more examples:
;
; * R75,D30,R83,U83,L12,D49,R71,U7,L72
;   U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
; * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;   U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
;
; What is the Manhattan distance from the central port to the closest intersection?
; (Answer: 2180)
;

(define (load-data filename)
  (let ((lines (file->lines filename))
        (data (make-hash))
        (line-idx 0))
    (for ((line lines))
         (set! line-idx (add1 line-idx))
         (hash-set! data line-idx line))
    data))

(define (fill-area! area line-idx data)
  (let ((x 0) (y 0)
              (steps (make-hash))
              (wire (string-split data ","))
              (step-total 1))
    (for ((datem wire))
         (let ((dir (substring datem 0 1))
               (dist (string->number (substring datem 1))))
           (for ((step (range dist)))
                (case dir
                  (("U") (set! y (sub1 y)))
                  (("R") (set! x (add1 x)))
                  (("D") (set! y (add1 y)))
                  (("L") (set! x (sub1 x))))
                (let ((coor (cons x y)))
                 (if (hash-has-key? area coor)
                     (when (not (member line-idx (hash-ref area coor)))
                       (hash-set! area coor (cons line-idx (hash-ref area coor))))
                     (hash-set! area coor (list line-idx)))
                 (when (not (hash-has-key? steps coor))
                   (hash-set! steps coor (+ step step-total)))))
           (set! step-total (+ step-total dist))))
    steps))

(define (manhat-dist coor)
  (+ (abs (car coor)) (abs (cdr coor))))

(define (part-one filename)
  (let* ((data (load-data filename))
         (area (make-hash))
         (origin (cons 0 0))
         (steps (make-hash))
         (tot-steps (lambda (coor) (+ (hash-ref (hash-ref steps 1) coor)
                                      (hash-ref (hash-ref steps 2) coor)))))
    (hash-set! area origin '())

    (for ((line-idx (hash-keys data)))
         (hash-set! area origin (cons line-idx (hash-ref area origin)))
         (hash-set! steps line-idx (fill-area! area line-idx (hash-ref data line-idx))))

    (printf "Part-two solution for file '~a': ~a~n"
            filename
            (foldl (lambda (coor acc) (if (< (tot-steps coor) acc) (tot-steps coor) acc)) 999999999
                   (filter (lambda (coor) (not (equal? coor (cons 0 0))))
                           (filter (lambda (coor) (> (length (hash-ref area coor)) 1))
                                   (hash-keys area)))))

    (foldl (lambda (coor acc) (if (< (manhat-dist coor) acc) (manhat-dist coor) acc)) 999999999
           (filter (lambda (coor) (not (equal? coor (cons 0 0))))
                   (filter (lambda (coor) (> (length (hash-ref area coor)) 1))
                           (hash-keys area))))))

(check-equal? (part-one "test1-data.txt") 6)
(check-equal? (part-one "test2-data.txt") 159)
(check-equal? (part-one "test3-data.txt") 135)

(check-equal? (part-one "day03-data.txt") 2180)

;
; --- Part Two ---
;
; It turns out that this circuit is very timing-sensitive; you actually
;  need to minimize the signal delay.
;
; To do this, calculate the number of steps each wire takes to reach each
;  intersection; choose the intersection where the sum of both wires' steps
;  is lowest. If a wire visits a position on the grid multiple times, use
;  the steps value from the first time it visits that position when calculating
;  the total value of a specific intersection.
;
; The number of steps a wire takes is the total number of grid squares the
;  wire has entered to get to that location, including the intersection being
;  considered. Again consider the example from above:
;
; ...........
; .+-----+...
; .|.....|...
; .|..+--X-+.
; .|..|..|.|.
; .|.-X--+.|.
; .|..|....|.
; .|.......|.
; .o-------+.
; ...........
;
; In the above example, the intersection closest to the central port is reached
;  after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the
;  second wire for a total of 20+20 = 40 steps.
;
; However, the top-right intersection is better: the first wire takes only 8+5+2
;  = 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.
;
; Here are the best steps for the extra examples from above:
;
; * R75,D30,R83,U83,L12,D49,R71,U7,L72
;   U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
;
; * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;   U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
;
; What is the fewest combined steps the wires must take to reach an intersection?
; (Answer: 112316)
;

; Answer solved within 'part-one' code above.

