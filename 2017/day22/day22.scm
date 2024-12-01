
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 22: Sporifica Virus ---
;
; Diagnostics indicate that the local grid computing cluster has been
;  contaminated with the Sporifica Virus. The grid computing cluster
;  is a seemingly-infinite two-dimensional grid of compute nodes. Each
;  node is either clean or infected by the virus.
;
; To prevent overloading the nodes (which would render them useless to
;  the virus) or detection by system administrators, exactly one virus
;  carrier moves through the network, infecting or cleaning nodes as
;  it moves. The virus carrier is always located on a single node in
;  the network (the current node) and keeps track of the direction it
;  is facing.
;
; To avoid detection, the virus carrier works in bursts; in each burst,
;  it wakes up, does some work, and goes back to sleep. The following
;  steps are all executed in order one time each burst:
;
; * If the current node is infected, it turns to its right. Otherwise,
;    it turns to its left. (Turning is done in-place; the current node
;    does not change.)
; * If the current node is clean, it becomes infected. Otherwise, it
;    becomes cleaned. (This is done after the node is considered for
;    the purposes of changing direction.)
; * The virus carrier moves forward one node in the direction it is facing.
;
; Diagnostics have also provided a map of the node infection status (your
;  puzzle input). Clean nodes are shown as .; infected nodes are shown as #.
;  This map only shows the center of the grid; there are many more nodes
;  beyond those shown, but none of them are currently infected.
;
; The virus carrier begins in the middle of the map facing up.
;
; For example, suppose you are given a map like this:
;
; ..#
; #..
; ...
;
; Then, the middle of the infinite grid looks like this, with the virus
;  carrier's position marked with [ ]:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . . #[.]. . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; The virus carrier is on a clean node, so it turns left, infects the node,
;  and moves left:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . .[#]# . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; The virus carrier is on an infected node, so it turns right, cleans the node,
;  and moves up:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . .[.]. # . . .
; . . . . # . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; Four times in a row, the virus carrier finds a clean, infects it, turns left,
;  and moves forward, ending in the same place and still facing up:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . #[#]. # . . .
; . . # # # . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; Now on the same node as before, it sees an infection, which causes it to turn
;  right, clean the node, and move forward:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . # .[.]# . . .
; . . # # # . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; After the above actions, a total of 7 bursts of activity had taken place. Of
;  them, 5 bursts of activity caused an infection.
;
; After a total of 70, the grid looks like this, with the virus carrier facing up:
;
; . . . . . # # . .
; . . . . # . . # .
; . . . # . . . . #
; . . # . #[.]. . #
; . . # . # . . # .
; . . . . . # # . .
; . . . . . . . . .
; . . . . . . . . .
;
; By this time, 41 bursts of activity caused an infection (though most of those
;  nodes have since been cleaned).
;
; After a total of 10000 bursts of activity, 5587 bursts will have caused an infection.
;
; Given your actual map, after 10000 bursts of activity, how many bursts cause a node
;  to become infected? (Do not count nodes that begin infected.)  (Answer: 5330)
;

(define infected #\#)
(define clean #\.)

(define infects 0)
(define cur-direction 'up)
(define cur-location (cons 0 0))
(define grid (make-hash))

(define (load-grid filename)
  (let ((lines (file->lines filename))
        (x 0) (y 0)
        (grid-width 0) (grid-height 0))
    (for ((line lines))
         (set! x 0)
         (for ((ch (string->list line)))
              (hash-set! grid (cons x y) ch)
              (set! x (add1 x)))
         (set! grid-width x)
         (set! y (add1 y)))
    (set! grid-height y)
    (set! cur-location (cons (floor (/ grid-width 2)) (floor (/ grid-height 2))))
    grid))

(define (new-location)
  (let ((x (car cur-location))
        (y (cdr cur-location)))
    (case cur-direction
      ((up)    (set! y (sub1 y)))
      ((down)  (set! y (add1 y)))
      ((left)  (set! x (sub1 x)))
      ((right) (set! x (add1 x)))
      (else (error "3")))
    (cons x y)))

(define (new-direction cur-node-ch)
  (if (char=? cur-node-ch infected)
      (case cur-direction ((up) 'right) ((down) 'left)  ((left) 'up)   ((right) 'down) (else (error "1")))
      (case cur-direction ((up) 'left)  ((down) 'right) ((left) 'down) ((right) 'up) (else (error "2")))))

(define (flip node)
  (case node
    ((#\#) #\.)
    ((#\.)
     (set! infects (add1 infects))
     #\#)
    (else (error "4"))))

(define (get-node)
  (when (not (hash-has-key? grid cur-location))
    (hash-set! grid cur-location clean))
  (hash-ref grid cur-location))

(define (burst)
  (let ((node (get-node)))
   (set! cur-direction (new-direction node))
   (hash-set! grid cur-location (flip node))
   (set! cur-location (new-location))))

(define (draw-grid)
  (let ((min-x 0) (max-x 0)
                  (min-y 0) (max-y 0))
    (for ((loc (hash-keys grid)))
         (when (< (car loc) min-x) (set! min-x (car loc)))
         (when (> (car loc) max-x) (set! max-x (car loc)))
         (when (< (cdr loc) min-y) (set! min-y (cdr loc)))
         (when (> (cdr loc) max-y) (set! max-y (cdr loc))))
    (for ((y (range min-y (add1 max-y))))
         (for ((x (range min-x (add1 max-x))))
              (if (hash-has-key? grid (cons x y))
                  (printf "~a~a"
                          (if (equal? cur-location (cons x y)) "[" " ")
                          (hash-ref grid (cons x y)))
                  (printf " .")))
         (printf "~n")))
  (printf "~nInfected: ~a~n" infects))

(define (part-one filename iterations)
  (reset)
  (set! grid (load-grid filename))
  (for ((i (range iterations)))
       (burst))
  (draw-grid)
  infects)

(define (reset)
  (set! infects 0)
  (set! cur-direction 'up)
  (set! cur-location (cons 0 0))
  (set! grid (make-hash)))

(check-equal? (part-one "test-data.txt" 10000) 5587)
(check-equal? (part-one "day22-data.txt" 10000) 5330)

;
; --- Part Two ---
;
; As you go to remove the virus from the infected nodes, it evolves to
;  resist your attempt.
;
; Now, before it infects a clean node, it will weaken it to disable your
;  defenses. If it encounters an infected node, it will instead flag the
;  node to be cleaned in the future. So:
;
; * Clean nodes become weakened.
; * Weakened nodes become infected.
; * Infected nodes become flagged.
; * Flagged nodes become clean.
;
; Every node is always in exactly one of the above states.
;
; The virus carrier still functions in a similar way, but now uses the
;  following logic during its bursts of action:
;
; * Decide which way to turn based on the current node:
; ** If it is clean, it turns left.
; ** If it is weakened, it does not turn, and will continue moving in the same direction.
; ** If it is infected, it turns right.
; ** If it is flagged, it reverses direction, and will go back the way it came.

; * Modify the state of the current node, as described above.

; * The virus carrier moves forward one node in the direction it is facing.
;
; Start with the same map (still using . for clean and # for infected) and
;  still with the virus carrier starting in the middle and facing up.
;
; Using the same initial state as the previous example, and drawing weakened
;  as W and flagged as F, the middle of the infinite grid looks like this,
;  with the virus carrier's position again marked with [ ]:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . . #[.]. . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; This is the same as before, since no initial nodes are weakened or flagged.
;  The virus carrier is on a clean node, so it still turns left, instead weakens
;  the node, and moves left:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . .[#]W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; The virus carrier is on an infected node, so it still turns right, instead flags
;  the node, and moves up:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . .[.]. # . . .
; . . . F W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; This process repeats three more times, ending on the previously-flagged
;  node and facing right:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . W W . # . . .
; . . W[F]W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; Finding a flagged node, it reverses direction and cleans the node:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . W W . # . . .
; . .[W]. W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; The weakened node becomes infected, and it continues in the same direction:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . W W . # . . .
; .[.]# . W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;
; Of the first 100 bursts, 26 will result in infection. Unfortunately, another
;  feature of this evolved virus is speed; of the first 10000000 bursts, 2511944
;  will result in infection.
;
; Given your actual map, after 10000000 bursts of activity, how many bursts
;  cause a node to become infected? (Do not count nodes that begin infected.)
;  (Answer: 2512103)
;

(define (new-direction cur-node-ch)
  (case cur-node-ch
    ((#\#) (case cur-direction ((up) 'right) ((down) 'left)  ((left) 'up)   ((right) 'down) (else (error "5"))))
    ((#\.) (case cur-direction ((up) 'left)  ((down) 'right) ((left) 'down) ((right) 'up) (else (error "6"))))
    ((#\W) cur-direction)
    ((#\F) (case cur-direction ((up) 'down)  ((down) 'up) ((left) 'right) ((right) 'left) (else (error "0"))))
    (else (error "8"))))

(define (flip node)
  (case node
    ((#\.) #\W)
    ((#\W)
     (set! infects (add1 infects))
     #\#)
    ((#\#) #\F)
    ((#\F) #\.)
    (else (error "7"))))

(check-equal? (part-one "test-data.txt" 100) 26)
(check-equal? (part-one "test-data.txt" 10000000) 2511944)
(check-equal? (part-one "day22-data.txt" 10000000) 2512103)

