
(require rackunit)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 18: Like a GIF For Your Yard ---
;
; After the million lights incident, the fire code has gotten stricter:
;  now, at most ten thousand lights are allowed. You arrange them in a
;  100x100 grid.
;
; Never one to let you down, Santa again mails you instructions on the
;  ideal lighting configuration. With so few lights, he says, you'll
;  have to resort to animation.
;
; Start by setting your lights to the included initial configuration
;  (your puzzle input). A # means "on", and a . means "off".
;
; Then, animate your grid in steps, where each step decides the next
;  configuration based on the current one. Each light's next state
;  (either on or off) depends on its current state and the current
;  states of the eight lights adjacent to it (including diagonals).
;  Lights on the edge of the grid might have fewer than eight neighbors;
;  the missing ones always count as "off".
;
; For example, in a simplified 6x6 grid, the light marked A has the
;  neighbors numbered 1 through 8, and the light marked B, which is
;  on an edge, only has the neighbors marked 1 through 5:
;
; 1B5...
; 234...
; ......
; ..123.
; ..8A4.
; ..765.
;
; The state a light should have next is based on its current state
;  (on or off) plus the number of neighbors that are on:
;
; * A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
; * A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
;
; All of the lights update simultaneously; they all consider the same
;  current state before moving to the next.
;
; Here's a few steps from an example configuration of another 6x6 grid:
;
; Initial state:
; .#.#.#
; ...##.
; #....#
; ..#...
; #.#..#
; ####..
;
; After 1 step:
; ..##..
; ..##.#
; ...##.
; ......
; #.....
; #.##..
;
; After 2 steps:
; ..###.
; ......
; ..###.
; ......
; .#....
; .#....
;
; After 3 steps:
; ...#..
; ......
; ...#..
; ..##..
; ......
; ......
;
; After 4 steps:
; ......
; ......
; ..##..
; ..##..
; ......
; ......
; After 4 steps, this example has four lights on.
;
; In your grid of 100x100 lights, given your initial configuration, how many lights are on after 100 steps?
;  (Answer: 814)
;

(define on #\#)
(define off #\.)
(define array-size 6)

(define (lights) (for/vector ([i (in-range array-size)]) (make-vector array-size #\.)))
(define (light-ref light-array x y) (vector-ref  (vector-ref light-array y) x))
(define (light-set! light-array x y val) (vector-set! (vector-ref light-array y) x val))

(define (light-show light-array)
  (for ((row light-array))
       (printf "~a~n" (list->string (vector->list row)))))

(define (neighbors x y)
  (let ((results '()))
   (for ((i '(-1 0 1)))
        (for ((j '(-1 0 1)))
             (let ((new-x (+ x i))
                   (new-y (+ y j)))
               (when (and
                       (not (and
                              (= new-x x)
                              (= new-y y)))
                       (>= new-x 0)
                       (>= new-y 0)
                       (< new-x array-size)
                       (< new-y array-size))
                 (set! results (cons (list (+ x i) (+ y j)) results))))))
   results))

(define (rule-result this-light num-neighbors-on)
  (if (char=? this-light on)
      (cond
        ((= num-neighbors-on 2) on)
        ((= num-neighbors-on 3) on)
        (else off))
      (cond
        ((= num-neighbors-on 3) on)
        (else off))
      ))

(define (count-neighbor-lights light-array x y)
  (let ((tot 0))
   (for ((xy (neighbors x y)))
        (let ((i (car xy))
              (j (cadr xy)))
          (when (char=? on (light-ref light-array i j))
            (set! tot (add1 tot)))))
   tot))

(define (evolve light-array)
  (let ((new-array (lights))) ; (vector-length light-array))))
   (for ((x array-size))
        (for ((y array-size))
             (light-set! new-array x y
                         (rule-result
                           (light-ref light-array x y)
                           (count-neighbor-lights light-array x y)))))
   new-array))

(define (part-one light-group steps)
  (let ((the-lights (vector-copy light-group)))
   (printf "~nInitial state:~n")
   (light-show the-lights)
   (for ((i steps))
        (set! the-lights (evolve the-lights))
        (printf "~nAfter ~a steps:~n" (add1 i))
        (light-show the-lights))
   (printf "~n")
   the-lights))

(define (get-light-group filename)
  (let ((new-light empty))
   (call-with-input-file
     filename
     (lambda (in-port)
       (do ((line (read-line in-port 'any)
                  (read-line in-port 'any))
            (idx 0 (add1 idx)))
           ((eof-object? line))

           (when (zero? idx)
             (set! new-light (make-vector (string-length line)))
             (vector-set! new-light 0 (list->vector (string->list line))))

           (vector-set! new-light idx (list->vector (string->list line))))))
   new-light))

(define (count-lights light-group)
  (let ((array-size (vector-length light-group))
        (tot 0))
    (for ((x array-size))
         (for ((y array-size))
              (when (char=? (light-ref light-group x y) on)
                (set! tot (add1 tot)))))
    tot))

(define array-size 6)
(define test-final-lights (part-one (get-light-group "test-data.txt") 4))
(define test-lights-on (count-lights test-final-lights))
(check-equal? test-lights-on 4)

(define array-size 100)
(define part-one-lights (part-one (get-light-group "day18-data.txt") 100))
(light-show part-one-lights)
(define lights-on (count-lights part-one-lights))
(check-equal? lights-on 814)

;
; --- Part Two ---
;
; You flip the instructions over; Santa goes on to point out that this is all
;  just an implementation of Conway's Game of Life. At least, it was, until you
;  notice that something's wrong with the grid of lights you bought: four lights,
;  one in each corner, are stuck on and can't be turned off. The example above
;  will actually run like this:
;
; Initial state:
; ##.#.#
; ...##.
; #....#
; ..#...
; #.#..#
; ####.#
;
; After 1 step:
; #.##.#
; ####.#
; ...##.
; ......
; #...#.
; #.####
;
; After 2 steps:
; #..#.#
; #....#
; .#.##.
; ...##.
; .#..##
; ##.###
;
; After 3 steps:
; #...##
; ####.#
; ..##.#
; ......
; ##....
; ####.#
;
; After 4 steps:
; #.####
; #....#
; ...#..
; .##...
; #.....
; #.#..#
;
; After 5 steps:
; ##.###
; .##..#
; .##...
; .##...
; #.#...
; ##...#
;
; After 5 steps, this example now has 17 lights on.
; 
; In your grid of 100x100 lights, given your initial configuration, but with the
;  four corners always in the on state, how many lights are on after 100 steps?
; (Answer: 924)
;

(define (turn-on-corners the-lights max-vec-idx)
  (light-set! the-lights 0 0 on)
  (light-set! the-lights 0 max-vec-idx on)
  (light-set! the-lights max-vec-idx 0 on)
  (light-set! the-lights max-vec-idx max-vec-idx on))

(define (part-two light-group steps)
  (let ((the-lights (vector-copy light-group))
        (max-vec-idx (sub1 array-size)))
    (turn-on-corners the-lights max-vec-idx)
    (printf "~nInitial state:~n")
    (light-show the-lights)
    (for ((i steps))
         (set! the-lights (evolve the-lights))
         (turn-on-corners the-lights max-vec-idx)
         (printf "~nAfter ~a steps:~n" (add1 i))
         (light-show the-lights))
    (printf "~n")
    the-lights))

(define array-size 6)
(define test-final-lights (part-two (get-light-group "test-data.txt") 5))
(define test-lights-on (count-lights test-final-lights))
(check-equal? test-lights-on 17)

(define array-size 100)
(define part-one-lights (part-two (get-light-group "day18-data.txt") 100))
(light-show part-one-lights)
(define lights-on (count-lights part-one-lights))
(check-equal? lights-on 924)

