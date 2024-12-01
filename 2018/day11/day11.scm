
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 11: Chronal Charge ---
;
; You watch the Elves and their sleigh fade into the distance as
;  they head toward the North Pole.
;
; Actually, you're the one fading. The falling sensation returns.
;
; The low fuel warning light is illuminated on your wrist-mounted
;  device. Tapping it once causes it to project a hologram of the
;  situation: a 300x300 grid of fuel cells and their current power
;  levels, some negative. You're not sure what negative power means
;  in the context of time travel, but it can't be good.
;
; Each fuel cell has a coordinate ranging from 1 to 300 in both the
;  X (horizontal) and Y (vertical) direction. In X,Y notation, the
;  top-left cell is 1,1, and the top-right cell is 300,1.
;
; The interface lets you select any 3x3 square of fuel cells. To
;  increase your chances of getting to your destination, you decide
;  to choose the 3x3 square with the largest total power.
;
; The power level in a given fuel cell can be found through the
;  following process:
;
; * Find the fuel cell's rack ID, which is its X coordinate plus 10.
; * Begin with a power level of the rack ID times the Y coordinate.
; * Increase the power level by the value of the grid serial number
;    (your puzzle input).
; * Set the power level to itself multiplied by the rack ID.
; * Keep only the hundreds digit of the power level (so 12345 becomes
;    3; numbers with no hundreds digit become 0).
; * Subtract 5 from the power level.
;
; For example, to find the power level of the fuel cell at 3,5 in a
;  grid with serial number 8:
;
; * The rack ID is 3 + 10 = 13.
; * The power level starts at 13 * 5 = 65.
; * Adding the serial number produces 65 + 8 = 73.
; * Multiplying by the rack ID produces 73 * 13 = 949.
; * The hundreds digit of 949 is 9.
; * Subtracting 5 produces 9 - 5 = 4.
;
; So, the power level of this fuel cell is 4.
;
; Here are some more example power levels:
;
; Fuel cell at  122,79, grid serial number 57: power level -5.
; Fuel cell at 217,196, grid serial number 39: power level  0.
; Fuel cell at 101,153, grid serial number 71: power level  4.
;
; Your goal is to find the 3x3 square which has the largest total power.
;  The square must be entirely within the 300x300 grid. Identify this
;  square using the X,Y coordinate of its top-left fuel cell. For example:
;
; For grid serial number 18, the largest total 3x3 square has a top-left
;  corner of 33,45 (with a total power of 29); these fuel cells appear in
;  the middle of this 5x5 region:
;
; -2  -4   4   4   4
; -4   4   4   4  -5
;  4   3   3   4  -4
;  1   1   2   4  -3
; -1   0   2  -5  -2
;
; For grid serial number 42, the largest 3x3 square's top-left is 21,61
;  (with a total power of 30); they are in the middle of this region:
;
; -3   4   2   2   2
; -4   4   3   3   4
; -5   3   3   4  -4
;  4   3   3   4  -3
;  3   3   3  -5  -1
;
; What is the X,Y coordinate of the top-left fuel cell of the 3x3 square
;  with the largest total power? (Answer: 34,13)
;
; Your puzzle input is 1723.
;

(define grid-size 300)

(define (hundreds n) (truncate (/ (modulo n 1000) 100)))

(define (power-level x y sn)
  (let* ((rack-id (+ 10 x))
         (power-lvl (* rack-id y))
         )
    (set! power-lvl (+ power-lvl sn))
    (set! power-lvl (* power-lvl rack-id))
    (- (hundreds power-lvl) 5)))

(check-equal? (power-level   3  5    8)  4)
(check-equal? (power-level 122 79   57) -5)
(check-equal? (power-level 217 196  39)  0)
(check-equal? (power-level 101 153  71)  4)

(define (create-grid sn)
  (let ((grid (make-hash)))
   (for ((y (range 1 (add1 grid-size))))
        (for ((x (range 1 (add1 grid-size))))
             (hash-set! grid (cons x y) (power-level x y sn))))
   grid))

(define (max-square grid)
  (let ((max-power 0)
        (max-coors #f))
   (for ((y (range 1 (- grid-size 2))))
        (for ((x (range 1 (- grid-size 2))))
             (define tot (+
                           (hash-ref grid (cons      x       y))
                           (hash-ref grid (cons (+ 1 x)      y))
                           (hash-ref grid (cons (+ 2 x)      y))
                           (hash-ref grid (cons      x  (+ 1 y)))
                           (hash-ref grid (cons (+ 1 x) (+ 1 y)))
                           (hash-ref grid (cons (+ 2 x) (+ 1 y)))
                           (hash-ref grid (cons      x  (+ 2 y)))
                           (hash-ref grid (cons (+ 1 x) (+ 2 y)))
                           (hash-ref grid (cons (+ 2 x) (+ 2 y)))))
             (when (> tot max-power)
               (set! max-power tot)
               (set! max-coors (cons x y)))))
   max-coors))

(define (part-one sn)
  (let ((grid (create-grid sn)))
   (max-square grid)))

(check-equal? (part-one   18) (cons 33 45))
(check-equal? (part-one   42) (cons 21 61))
(check-equal? (part-one 1723) (cons 34 13))

;
; -- Part Two ---
;
; You discover a dial on the side of the device; it seems to let you
;  select a square of any size, not just 3x3. Sizes from 1x1 to 300x300
;  are supported.
;
; Realizing this, you now must find the square of any size with the
;  largest total power. Identify this square by including its size as
;  a third parameter after the top-left coordinate: a 9x9 square with
;  a top-left corner of 3,5 is identified as 3,5,9.
;
; For example:
;
; * For grid serial number 18, the largest total square (with a total power
;    of 113) is 16x16 and has a top-left corner of 90,269, so its identifier
;    is 90,269,16.
; * For grid serial number 42, the largest total square (with a total power
;    of 119) is 12x12 and has a top-left corner of 232,251, so its identifier
;    is 232,251,12.
;
; What is the X,Y,size identifier of the square with the largest total power?
; (Answer: 280,251,11) (Very slow, so just used unchanging value up 20 101).
;

(define (sum-size grid x y size)
  (let ((tot 0))
   (for ((y-off (range size)))
        (for ((x-off (range size)))
             (set! tot (+ tot (hash-ref grid (cons (+ x x-off) (+ y y-off)))))))
   tot))

(check-equal? (sum-size (create-grid 18) 33 45 3) 29)
(check-equal? (sum-size (create-grid 42) 21 61 3) 30)

(define (part-two sn)
  (let ((grid (create-grid sn))
        (max-power 0)
        (max-coors #f)
        (max-size  0)
        (g-size 12))
    (for ((g-size (range 3 (add1 grid-size))))
         (printf "Hi Edward A, g-size:~a, one to:~a~n" g-size (- grid-size g-size))
         (for ((y (range 1 (- grid-size g-size))))
              (for ((x (range 1 (- grid-size g-size))))
                   (define tot (sum-size grid x y g-size))
                   (when (> tot max-power)
                     (set! max-power tot)
                     (set! max-size  g-size)
                     (set! max-coors (cons x y))
                     (printf "Hi Edward B, max-power:~a, max-size:~a, max-coors:~a~n" max-power max-size max-coors)
                     ))))
    (cons max-coors max-size)))

;(check-equal? (part-two 18) (cons (cons  90 269) 16))
;(check-equal? (part-two 42) (cons (cons 232 251) 12))

(check-equal? (part-two 1723) (cons (cons 1 1) 1))

