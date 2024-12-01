
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 3: Spiral Memory ---
;
; You come across an experimental new kind of memory stored on an infinite
;  two-dimensional grid.
;
; Each square on the grid is allocated in a spiral pattern starting at a
;  location marked 1 and then counting up while spiraling outward. For example,
;  the first few squares are allocated like this:
;
; 17  16  15  14  13
; 18   5   4   3  12
; 19   6   1   2  11
; 20   7   8   9  10
; 21  22  23---> ...
;
; While this is very space-efficient (no squares are skipped), requested data
;  must be carried back to square 1 (the location of the only access port for
;  this memory system) by programs that can only move up, down, left, or right.
;  They always take the shortest path: the Manhattan Distance between the location
;  of the data and square 1.
;
; For example:
;
; * Data from square 1 is carried 0 steps, since it's at the access port.
; * Data from square 12 is carried 3 steps, such as: down, left, left.
; * Data from square 23 is carried only 2 steps: up twice.
; * Data from square 1024 must be carried 31 steps.
;
; How many steps are required to carry the data from the square identified in
;  your puzzle input all the way to the access port? (Answer: 438)
;
; Your puzzle input is 265149.
;

(define puzzle-input 265149)

(define (fill-grid limit)
  (define grid (make-hash))

  (define (fill-helper step dir x y num lim)
;    (printf "fill-helper, step: ~a, dir: ~a, x: ~a, y: ~a, num: ~a, lim: ~a~n" step dir x y num lim)
    (cond
      ((> num lim) grid)
      ((equal? dir 'right)
       (for ((s (range step)))
            (hash-set! grid (cons (+ s x) y) (+ num s)))
       (fill-helper step 'up (+ step x) y (+ num step) lim))

      ((equal? dir 'up)
       (for ((s (range step)))
            (hash-set! grid (cons x (- y s)) (+ num s)))
       (fill-helper (add1 step) 'left x (- y step) (+ num step) lim))

      ((equal? dir 'left)
       (for ((s (range step)))
            (hash-set! grid (cons (- x s) y) (+ num s)))
       (fill-helper step 'down (- x step) y (+ num step) lim))

      ((equal? dir 'down)
       (for ((s (range step)))
            (hash-set! grid (cons x (+ s y)) (+ num s)))
       (fill-helper (add1 step) 'right x (+ y step) (+ num step) lim))))

  (fill-helper 1 'right 0 0 1 limit))

;(fill-grid puzzle-input)

; Solved Part One by dumping to the screen and searching for number

;
; --- Part Two ---
;
; As a stress test on the system, the programs here clear the grid and then
;  store the value 1 in square 1. Then, in the same allocation order as shown
;  above, they store the sum of the values in all adjacent squares, including
;  diagonals.
;
; So, the first few squares' values are chosen as follows:
;
; * Square 1 starts with the value 1.
; * Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
; * Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
; * Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
; * Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
;
; Once a square is written, its value does not change. Therefore, the first few
;  squares would receive the following values:
;
; 147  142  133  122   59
; 304    5    4    2   57
; 330   10    1    1   54
; 351   11   23   25   26
; 362  747  806--->   ...
;
; What is the first value written that is larger than your puzzle input? (Answer: 266330)
;

(define (part-two limit)
  (define grid (make-hash))

  (define (neighbor-sum x y)
    (let ((a (cons (sub1 x) (sub1 y)))
          (b (cons       x  (sub1 y)))
          (c (cons (add1 x) (sub1 y)))  ; a  b  c
          (d (cons (sub1 x)       y))   ; d  e  f
          (e (cons       x        y))   ; g  h  i
          (f (cons (add1 x)       y))
          (g (cons (sub1 x) (add1 y)))
          (h (cons       x  (add1 y)))
          (i (cons (add1 x) (add1 y))))
      (+
        (if (hash-has-key? grid a) (hash-ref grid a) 0)
        (if (hash-has-key? grid b) (hash-ref grid b) 0)
        (if (hash-has-key? grid c) (hash-ref grid c) 0)
        (if (hash-has-key? grid d) (hash-ref grid d) 0)
        (if (hash-has-key? grid e) (hash-ref grid e) 0)
        (if (hash-has-key? grid f) (hash-ref grid f) 0)
        (if (hash-has-key? grid g) (hash-ref grid g) 0)
        (if (hash-has-key? grid h) (hash-ref grid h) 0)
        (if (hash-has-key? grid i) (hash-ref grid i) 0))))

  (define (fill-helper step dir x y num lim)
    (cond
      ((> (neighbor-sum x y) lim) grid)
      ((equal? dir 'right)
       (for ((s (range step)))
            (hash-set! grid (cons (+ s x) y) (neighbor-sum (+ s x) y)))
       (fill-helper step 'up (+ step x) y (+ num step) lim))

      ((equal? dir 'up)
       (for ((s (range step)))
            (hash-set! grid (cons x (- y s)) (neighbor-sum x (- y s))))
       (fill-helper (add1 step) 'left x (- y step) (+ num step) lim))

      ((equal? dir 'left)
       (for ((s (range step)))
            (hash-set! grid (cons (- x s) y) (neighbor-sum (- x s) y)))
       (fill-helper step 'down (- x step) y (+ num step) lim))

      ((equal? dir 'down)
       (for ((s (range step)))
            (hash-set! grid (cons x (+ s y)) (neighbor-sum x (+ s y))))
       (fill-helper (add1 step) 'right x (+ y step) (+ num step) lim))))

  (hash-set! grid (cons 0 0) 1)
  (fill-helper 1 'right 0 0 1 limit))

;(part-two puzzle-input)
; Solved by dumping and sorting grid.

