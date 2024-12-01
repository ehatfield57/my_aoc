
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 13: A Maze of Twisty Little Cubicles ---
;
; You arrive at the first floor of this new building to discover a much less
;  welcoming environment than the shiny atrium of the last one. Instead, you
;  are in a maze of twisty little cubicles, all alike.
;
; Every location in this area is addressed by a pair of non-negative integers
;  (x,y). Each such coordinate is either a wall or an open space. You can't move
;  diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward
;  positive x and y; negative values are invalid, as they represent a location
;  outside the building. You are in a small waiting area at 1,1.
;
; While it seems chaotic, a nearby morale-boosting poster explains, the layout is
;  actually quite logical. You can determine whether a given x,y coordinate will
;  be a wall or an open space using a simple system:
;
; * Find x*x + 3*x + 2*x*y + y + y*y.
; * Add the office designer's favorite number (your puzzle input).
; * Find the binary representation of that sum; count the number of bits that are 1.
;   * If the number of bits that are 1 is even, it's an open space.
;   * If the number of bits that are 1 is odd, it's a wall.
;
; For example, if the office designer's favorite number were 10, drawing walls as #
;  and open spaces as ., the corner of the building containing 0,0 would look like
;  this:
;
;   0123456789
; 0 .#.####.##
; 1 ..#..#...#
; 2 #....##...
; 3 ###.#.###.
; 4 .##..#..#.
; 5 ..##....#.
; 6 #...##.###
;
; Now, suppose you wanted to reach 7,4. The shortest route you could take is marked as O:
;
;   0123456789
; 0 .#.####.##
; 1 .O#..#...#
; 2 #OOO.##...
; 3 ###O#.###.
; 4 .##OO#OO#.
; 5 ..##OOO.#.
; 6 #...##.###
;
; Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current location, 1,1).
;
; What is the fewest number of steps required for you to reach 31,39? (Answer: 96)
;

(define puzzle-input 1358)
(define test-input 10)

(define (maze-char favorite x y)
  (let* ((bin-string (lambda (n) (number->string n 2)))
         (inc-ones   (lambda (c acc) (if (equal? c #\1) (add1 acc) acc)))
         (function   (lambda (x y) (+ (* x x) (* 3 x) (* 2 x y) y (* y y))))
         (count-ones (lambda (s) (foldl inc-ones 0 (string->list s)))))
    (if (even? (count-ones (bin-string (+ favorite (function x y))))) #\. #\#)))

(define (draw-maze width height favorite goal-x goal-y)
  (let ((op1 (open-output-string)))
   (for ((y (range height)))
        (for ((x (range width)))
             (write-string (cond
                            ((and (= x 1) (= y 1)) "*")
                            ((and (= x goal-x) (= y goal-y)) "O")
                            (else (string (maze-char favorite x y)))) op1))
        (write-string "\n" op1))
   (write-string "\n" op1)
   (get-output-string op1)))

(define (maze->hash maze-string)
  (let ((maze (make-hash))
        (x 0) (y 0))
    (for ((chr (string->list maze-string)))
         (case chr
           ((#\*) (hash-set! maze (cons x y) chr)
                  (hash-set! maze 'start (cons x y)))
           ((#\O) (hash-set! maze (cons x y) chr)
                  (hash-set! maze 'goal (cons x y)))
           ((#\newline) (set! x -1)
                        (set! y (add1 y)))
           (else (hash-set! maze (cons x y) chr)))
         (set! x (add1 x)))
    maze))

(define (find-directions maze x y)
  (let ((directions '())
        (offsets (list (cons       x  (sub1 y)) 
                       (cons (sub1 x)       y)  
                       (cons (add1 x)       y)  
                       (cons x        (add1 y)))))
    (for ((test-key offsets))
         (when (and (hash-has-key? maze test-key)
                    (not (char=? (hash-ref maze test-key) #\#)))
           (set! directions (cons test-key directions))))
    directions))

(define (solve-helper maze visited directions depth)
  (printf "Hi Edward Entering solve-helper, depth: ~a, directions: ~a~n" depth directions)
  (let ((new-directions '()))
   (for ((a-dir directions))
        (when (not (hash-has-key? visited a-dir))
          (hash-set! visited a-dir depth)
          (set! new-directions (append (find-directions maze (car a-dir) (cdr a-dir)) new-directions))))
   (when (> (length new-directions) 0)
     (solve-helper maze visited new-directions (add1 depth)))))

(define (solve-maze maze-string)
  (let ((maze (maze->hash maze-string)) 
        (visited (make-hash)))
    (solve-helper maze visited (list (hash-ref maze 'start)) 0)
    (printf "Hi Edward A, maze-string:~n~a~n" maze-string)
    (printf "Hi Edward B, maze: ~a~n" maze)
    (printf "Hi Edward C, visited: ~a~n" visited)
    (hash-ref visited (hash-ref maze 'goal))))

;(define maze (draw-maze 10 7 test-input 7 4))
;(printf "Steps to solve example maze: ~a~n" (solve-maze maze))
;(check-equal? (solve-maze maze) 11)

(printf "Part One Maze:~n")
(define maze (draw-maze 40 45 puzzle-input 31 39))
(printf "Steps to solve maze: ~a~n" (solve-maze maze))

;
; --- Part Two ---
;
; How many locations (distinct x,y coordinates, including your starting location)
;  can you reach in at most 50 steps?  (Answer: 141)
;

; Solved by dumping out 'visited' hash, sorting by depth and counting depth <= 50

