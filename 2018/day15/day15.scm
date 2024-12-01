
(require rackunit)
(require graph)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 15: Beverage Bandits ---
;
; Having perfected their hot chocolate, the Elves have a new problem: the
;  Goblins that live in these caves will do anything to steal it. Looks like
;  they're here for a fight.
;
; You scan the area, generating a map of the walls (#), open cavern (.), and
;  starting position of every Goblin (G) and Elf (E) (your puzzle input).
;
; Combat proceeds in rounds; in each round, each unit that is still alive
;  takes a turn, resolving all of its actions before the next unit's turn
;  begins. On each unit's turn, it tries to move into range of an enemy (if it
;  isn't already) and then attack (if it is in range).
;
; All units are very disciplined and always follow very strict combat rules.
;  Units never move or attack diagonally, as doing so would be dishonorable.
;  When multiple choices are equally valid, ties are broken in reading order:
;  top-to-bottom, then left-to-right. For instance, the order in which units
;  take their turns within a round is the reading order of their starting
;  positions in that round, regardless of the type of unit or whether other
;  units have moved after the round started. For example:
;
;                  would take their
; These units:   turns in this order:
;   #######           #######
;   #.G.E.#           #.1.2.#
;   #E.G.E#           #3.4.5#
;   #.G.E.#           #.6.7.#
;   #######           #######
;
; Each unit begins its turn by identifying all possible targets (enemy units).
;  If no targets remain, combat ends.
;
; Then, the unit identifies all of the open squares (.) that are in range of
;  each target; these are the squares which are adjacent (immediately up,
;  down, left, or right) to any target and which aren't already occupied by a
;  wall or another unit. Alternatively, the unit might already be in range of
;  a target. If the unit is not already in range of a target, and there are no
;  open squares which are in range of a target, the unit ends its turn.
;
; If the unit is already in range of a target, it does not move, but continues
;  its turn with an attack. Otherwise, since it is not in range of a target, it
;  moves.
;
; To move, the unit first considers the squares that are in range and determines
;  which of those squares it could reach in the fewest steps. A step is a single
;  movement to any adjacent (immediately up, down, left, or right) open (.) square.
;  Units cannot move into walls or other units. The unit does this while considering
;  the current positions of units and does not do any prediction about where units
;  will be later. If the unit cannot reach (find an open path to) any of the squares
;  that are in range, it ends its turn. If multiple squares are in range and tied
;  for being reachable in the fewest steps, the square which is first in reading
;  order is chosen. For example:
;
; Targets:      In range:     Reachable:    Nearest:      Chosen:
; #######       #######       #######       #######       #######
; #E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
; #...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
; #.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
; #######       #######       #######       #######       #######
;
; In the above scenario, the Elf has three targets (the three Goblins):
;
; * Each of the Goblins has open, adjacent squares which are in range
;    (marked with a ? on the map).
; * Of those squares, four are reachable (marked @); the other two (on the
;    right) would require moving through a wall or unit to reach.
; * Three of these reachable squares are nearest, requiring the fewest
;    steps (only 2) to reach (marked !).
; * Of those, the square which is first in reading order is chosen (+).
;
; The unit then takes a single step toward the chosen square along the
;  shortest path to that square. If multiple steps would put the unit
;  equally closer to its destination, the unit chooses the step which;
;  is first in reading order. (This requires knowing when there is more
;  than one shortest path so that you can consider the first step of each
;  such path.) For example:
;
; In range:     Nearest:      Chosen:       Distance:     Step:
; #######       #######       #######       #######       #######
; #.E...#       #.E...#       #.E...#       #4E212#       #..E..#
; #...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
; #..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
; #######       #######       #######       #######       #######
;
; The Elf sees three squares in range of a target (?), two of which are
;  nearest (!), and so the first in reading order is chosen (+). Under
;  "Distance", each open square is marked with its distance from the
;  destination square; the two squares to which the Elf could move on
;  this turn (down and to the right) are both equally good moves and
;  would leave the Elf 2 steps from being in range of the Goblin. Because
;  the step which is first in reading order is chosen, the Elf moves right
;  one square.
;
; Here's a larger example of movement:
;
; Initially:
; #########
; #G..G..G#
; #.......#
; #.......#
; #G..E..G#
; #.......#
; #.......#
; #G..G..G#
; #########
;
; After 1 round:
; #########
; #.G...G.#
; #...G...#
; #...E..G#
; #.G.....#
; #.......#
; #G..G..G#
; #.......#
; #########
;
; After 2 rounds:
; #########
; #..G.G..#
; #...G...#
; #.G.E.G.#
; #.......#
; #G..G..G#
; #.......#
; #.......#
; #########
;
; After 3 rounds:
; #########
; #.......#
; #..GGG..#
; #..GEG..#
; #G..G...#
; #......G#
; #.......#
; #.......#
; #########
;
; Once the Goblins and Elf reach the positions above, they all are either in
;  range of a target or cannot find any square in range of a target, and so
;  none of the units can move until a unit dies.
;
; After moving (or if the unit began its turn in range of a target), the unit
;  attacks.
;
; To attack, the unit first determines all of the targets that are in range
;  of it by being immediately adjacent to it. If there are no such targets,
;  the unit ends its turn. Otherwise, the adjacent target with the fewest hit
;  points is selected; in a tie, the adjacent target with the fewest hit
;  points which is first in reading order is selected.
;
; The unit deals damage equal to its attack power to the selected target,
;  reducing its hit points by that amount. If this reduces its hit points to 0
;  or fewer, the selected target dies: its square becomes . and it takes no
;  further turns.
;
; Each unit, either Goblin or Elf, has 3 attack power and starts with 200 hit
;  points.
;
; For example, suppose the only Elf is about to attack:
;
;        HP:            HP:
; G....  9       G....  9  
; ..G..  4       ..G..  4  
; ..EG.  2  -->  ..E..     
; ..G..  2       ..G..  2  
; ...G.  1       ...G.  1  
;
; The "HP" column shows the hit points of the Goblin to the left in the corresponding
;  row. The Elf is in range of three targets: the Goblin above it (with 4 hit points),
;  the Goblin to its right (with 2 hit points), and the Goblin below it (also with 2
;  hit points). Because three targets are in range, the ones with the lowest hit points
;  are selected: the two Goblins with 2 hit points each (one to the right of the Elf
;  and one below the Elf). Of those, the Goblin first in reading order (the one to the
;  right of the Elf) is selected. The selected Goblin's hit points (2) are reduced by
;  the Elf's attack power (3), reducing its hit points to -1, killing it.
;
; After attacking, the unit's turn ends. Regardless of how the unit's turn ends, the
;  next unit in the round takes its turn. If all units have taken turns in this round,
;  the round ends, and a new round begins.
;
; The Elves look quite outnumbered. You need to determine the outcome of the battle:
;  the number of full rounds that were completed (not counting the round in which
;  combat ends) multiplied by the sum of the hit points of all remaining units at the
;  moment combat ends. (Combat only ends when a unit finds no targets during its turn.)
;
; Below is an entire sample combat. Next to each map, each row's units' hit points
;  are listed from left to right.
;
; Initially:
; #######   
; #.G...#   G(200)
; #...EG#   E(200), G(200)
; #.#.#G#   G(200)
; #..G#E#   G(200), E(200)
; #.....#   
; #######   
;
; After 1 round:
; #######   
; #..G..#   G(200)
; #...EG#   E(197), G(197)
; #.#G#G#   G(200), G(197)
; #...#E#   E(197)
; #.....#   
; #######   
;
; After 2 rounds:
; #######   
; #...G.#   G(200)
; #..GEG#   G(200), E(188), G(194)
; #.#.#G#   G(194)
; #...#E#   E(194)
; #.....#   
; #######   
;
; Combat ensues; eventually, the top Elf dies:
; 
; After 23 rounds:
; #######   
; #...G.#   G(200)
; #..G.G#   G(200), G(131)
; #.#.#G#   G(131)
; #...#E#   E(131)
; #.....#   
; #######   
;
; After 24 rounds:
; #######   
; #..G..#   G(200)
; #...G.#   G(131)
; #.#G#G#   G(200), G(128)
; #...#E#   E(128)
; #.....#   
; #######   
;
; After 25 rounds:
; #######   
; #.G...#   G(200)
; #..G..#   G(131)
; #.#.#G#   G(125)
; #..G#E#   G(200), E(125)
; #.....#   
; #######   
;
; After 26 rounds:
; #######   
; #G....#   G(200)
; #.G...#   G(131)
; #.#.#G#   G(122)
; #...#E#   E(122)
; #..G..#   G(200)
; #######   
;
; After 27 rounds:
; #######   
; #G....#   G(200)
; #.G...#   G(131)
; #.#.#G#   G(119)
; #...#E#   E(119)
; #...G.#   G(200)
; #######   
;
; After 28 rounds:
; #######   
; #G....#   G(200)
; #.G...#   G(131)
; #.#.#G#   G(116)
; #...#E#   E(113)
; #....G#   G(200)
; #######   
;
; More combat ensues; eventually, the bottom Elf dies:
;
; After 47 rounds:
; #######   
; #G....#   G(200)
; #.G...#   G(131)
; #.#.#G#   G(59)
; #...#.#   
; #....G#   G(200)
; #######   
;
; Before the 48th round can finish, the top-left Goblin finds that there are
;  no targets remaining, and so combat ends. So, the number of full rounds
;  that were completed is 47, and the sum of the hit points of all remaining
;  units is 200+131+59+200 = 590. From these, the outcome of the battle is
;  47 * 590 = 27730.
;
; Here are a few example summarized combats:
;
; #######       #######
; #G..#E#       #...#E#   E(200)
; #E#E.E#       #E#...#   E(197)
; #G.##.#  -->  #.E##.#   E(185)
; #...#E#       #E..#E#   E(200), E(200)
; #...E.#       #.....#
; #######       #######
;
; Combat ends after 37 full rounds
; Elves win with 982 total hit points left
; Outcome: 37 * 982 = 36334
;
; #######       #######   
; #E..EG#       #.E.E.#   E(164), E(197)
; #.#G.E#       #.#E..#   E(200)
; #E.##E#  -->  #E.##.#   E(98)
; #G..#.#       #.E.#.#   E(200)
; #..E#.#       #...#.#   
; #######       #######   
;
; Combat ends after 46 full rounds
; Elves win with 859 total hit points left
; Outcome: 46 * 859 = 39514
;
; #######       #######   
; #E.G#.#       #G.G#.#   G(200), G(98)
; #.#G..#       #.#G..#   G(200)
; #G.#.G#  -->  #..#..#   
; #G..#.#       #...#G#   G(95)
; #...E.#       #...G.#   G(200)
; #######       #######   
;
; Combat ends after 35 full rounds
; Goblins win with 793 total hit points left
; Outcome: 35 * 793 = 27755
;
; #######       #######   
; #.E...#       #.....#   
; #.#..G#       #.#G..#   G(200)
; #.###.#  -->  #.###.#   
; #E#G#G#       #.#.#.#   
; #...#G#       #G.G#G#   G(98), G(38), G(200)
; #######       #######   
;
; Combat ends after 54 full rounds
; Goblins win with 536 total hit points left
; Outcome: 54 * 536 = 28944
;
; #########       #########   
; #G......#       #.G.....#   G(137)
; #.E.#...#       #G.G#...#   G(200), G(200)
; #..##..G#       #.G##...#   G(200)
; #...##..#  -->  #...##..#   
; #...#...#       #.G.#...#   G(200)
; #.G...G.#       #.......#   
; #.....G.#       #.......#   
; #########       #########   
;
; Combat ends after 20 full rounds
; Goblins win with 937 total hit points left
; Outcome: 20 * 937 = 18740
;
; What is the outcome of the combat described in your puzzle input?
;  (Answer: ?)
;

(struct unit (type alive attack hps) #:transparent #:mutable)

(define new-goblin (unit 'goblin  #t 3 200))
(define new-elf    (unit 'elf     #t 3 200))

(define (reading-order p1 p2)
  (cond
    [(< (cdr p1) (cdr p2)) #t] 
    [(> (cdr p1) (cdr p2)) #f] 
    [else (< (car p1) (car p2))]))

(define (load-cave filename)
  (let ((cave (make-hash))
        (units (make-hash))
        (input (open-input-file filename))
        (x 0) (y 0))
    (let loop ((ch (read-char input)))
     (case ch
       ((#\# #\.)
        (hash-set! cave (cons x y) ch)
        (set! x (add1 x)))
       ((#\G)
        (hash-set! units (cons x y) new-goblin)
        (hash-set! cave (cons x y) #\.)
        (set! x (add1 x)))
       ((#\E)
        (hash-set! units (cons x y) new-elf)
        (hash-set! cave (cons x y) #\.)
        (set! x (add1 x)))
       ((#\newline)
        (set! y (add1 y)) 
        (set! x 0))
       )
     (set! ch (read-char input))
     (when (not (eof-object? ch)) (loop ch)))
    (close-input-port input)

    (values cave units)))

(define (draw-cave cave units (highlights '()))
  (let ((max-x (apply max (map car (hash-keys cave))))
        (max-y (apply max (map cdr (hash-keys cave)))))
    (for ((y (range (add1 max-y))))
         (for ((x (range (add1 max-x))))
              (if (hash-has-key? units (cons x y))
                  (cond
                    ((eq? (unit-type (hash-ref units (cons x y))) 'goblin) (printf "G"))
                    ((eq? (unit-type (hash-ref units (cons x y))) 'elf) (printf "E")))
                  (cond
                    ((member (cons x y) highlights) (printf "+"))
                    (else (printf "~a" (hash-ref cave (cons x y)))))))
         (printf "~n"))))

(define (get-units units type)
  (let ((return-units '()))
   (for ((coor (hash-keys units)))
        (define unit (hash-ref units coor))
        (when (equal? (unit-type unit) type)
          (set! return-units (cons unit return-units))))
   return-units))

(define (get-target-coors unit units)
  (let* ((return-coors '())
         (enemy-type (if (equal? (unit-type unit) 'elf) 'goblin 'elf)))
    (for ((coor (hash-keys units)))
         (when (equal? (unit-type (hash-ref units coor)) enemy-type)
           (set! return-coors (cons coor return-coors))))
    return-coors))

(define (get-nsew-coors coor)
  (let ((x (car coor))
        (y (cdr coor)))
  (list (cons       x (sub1 y))
        (cons       x (add1 y))
        (cons (add1 x)      y)
        (cons (sub1 x)      y))))

(define (coor-empty? coor cave units)
  (and (hash-has-key? cave coor)
       (char=? #\. (hash-ref cave coor))
       (not (hash-has-key? units coor))))

(define (get-in-range-coors unit-coor target-coors cave units)
  (let* ((in-range-coors '()))
         (for ((t-coor target-coors))
              (let ((nsew (get-nsew-coors t-coor)))
               (for ((nsew-coor nsew))
                    (when (coor-empty? nsew-coor cave units)
                      (set! in-range-coors (cons nsew-coor in-range-coors))))))
    in-range-coors))

(define (get-available-floor-coors cave units)
  (let ((coors '())
        (unit-coors (hash-keys units)))
    (for ((coor (hash-keys cave)))
         (when (and (char=? #\. (hash-ref cave coor))
                    (not (member coor unit-coors)))
           (set! coors (cons coor coors))))
    coors))

(define (convert-to-graph available-floor-coors unit-coor)
  (let ((lookup (make-hash))
        (vertices '()))

    (hash-set! lookup unit-coor '())
    (for ((coor available-floor-coors)) ; #hash((1 . 1) => '(), ...)
         (hash-set! lookup coor '()))

    (for ((coor available-floor-coors))
         (for ((maybe (get-nsew-coors coor)))
              (when (hash-has-key? lookup maybe) ; #hash((1 . 1) => '((2 . 1), ...)
                (hash-set! lookup coor (cons maybe (hash-ref lookup coor))))))

    (for ((maybe (get-nsew-coors unit-coor)))
         (when (hash-has-key? lookup maybe)
           (hash-set! lookup unit-coor (cons maybe (hash-ref lookup unit-coor)))))

    (for ((coor-from available-floor-coors))
         (for ((coor-to (hash-ref lookup coor-from)))
              (define edge (list coor-from coor-to))
              (set! vertices (cons edge vertices))))
    (unweighted-graph/undirected vertices)))

(define (choose-nearest-coor unit-coor in-range-coors cave units)
;  (printf "Hi Edward D, entering choose-nearest-coor, unit-coor:~a, in-range-coors:~a~n" unit-coor in-range-coors)
  (let* ((available-floor-coors (get-available-floor-coors cave units))
         (graph-info (convert-to-graph available-floor-coors unit-coor)))
;    (printf "Hi Edward E, available-floor-coors:~a~n" available-floor-coors)
    (define-values (distances ignore) (bfs graph-info unit-coor))
;    (printf "Hi Edward F, distances:~a~n" distances)
    (define reachable (filter (lambda (coor) (and (member coor in-range-coors)
                                                  (not (equal? (hash-ref distances coor) +inf.0))))
                              (hash-keys distances)))
;    (printf "Hi Edward G, reachable:~a~n" reachable)
    (if (null? reachable)
        #f
        (let* ((shortest-distance (apply min (map (lambda (key) (hash-ref distances key)) reachable)))
               (shortest-coors (filter (lambda (key) (= shortest-distance (hash-ref distances key))) reachable))
               (goal-coor (car (sort shortest-coors reading-order)))
               (src-coors (filter (lambda (coor) (member coor available-floor-coors)) (get-nsew-coors unit-coor))))
;          (printf "Hi Edward H, shortest-distance:~a~n" shortest-distance)
;          (printf "Hi Edward I, shortest-coors:~a~n" shortest-coors)
;          (printf "Hi Edward J, goal-coor:~a~n" goal-coor)
;          (printf "Hi Edward J1, new nsew coors from unit-coor:~a~n" (get-nsew-coors unit-coor))
;          (printf "Hi Edward J2, member:~a~n" (member (cons 3 1) available-floor-coors))
;          (printf "Hi Edward J3, src-coors:~a~n" src-coors)
          (define-values (distances-back ignore) (bfs graph-info goal-coor))
;          (printf "Hi Edward M, distances back from goal:~a~n" distances-back)
          (define back-dist (apply min (map (lambda (key) (hash-ref distances-back key)) src-coors)))
;          (printf "Hi Edward N, back-dist:~a~n" back-dist)
          (define back-coors (filter (lambda (key) (= back-dist (hash-ref distances-back key))) src-coors))
;          (printf "Hi Edward O, back-coors:~a~n" back-coors)
          (define back-sort (sort back-coors reading-order))
;          (printf "Hi Edward P, back-sort:~a~n" back-sort)
          (car back-sort)))))

(define (get-unit-move unit-coor cave units)
;  (printf "Hi Edward A, entering get-unit-move, unit-coor:~a~n" unit-coor)
  (let* ((unit (hash-ref units unit-coor))
         (target-coors (get-target-coors unit units))
         (in-range-coors (get-in-range-coors unit-coor target-coors cave units))
         (move-to-coor (choose-nearest-coor unit-coor in-range-coors cave units)))
;    (printf "Hi Edward B, in-range-coors:~a~n" in-range-coors)
;    (printf "Hi Edward C, move-to-coor:~a~n" move-to-coor)
    move-to-coor))

(define (enemy-in-range units unit-coor enemy)
  (let* ((coors (get-nsew-coors unit-coor))
         (enemy-coors (filter (lambda (coor) (and (hash-has-key? units coor)
                                                  (equal? enemy (unit-type (hash-ref units coor)))))
                              coors)))
    (if (null? enemy-coors) #f enemy-coors)))


(define (combat units unit-coor enemies-in-range)
  (define (hp-order p1 p2)
    (< (unit-hps (hash-ref units p1)) (unit-hps (hash-ref units p2))))

  (let ((attacker (hash-ref units unit-coor)))
   (printf "Hi Edward a, entering combat, unit-coor:~a, attacker:~a, enemies-in-range:~a~n" unit-coor attacker enemies-in-range)
   (define enemy-coor (car (sort enemies-in-range hp-order)))
   (printf "Hi Edward aa, enemy-coor:~a~n" enemy-coor)
   (let ((enemy (hash-ref units enemy-coor)))
    (printf "Hi Edward b, combat between attacker:~a and enemy:~a~n" attacker enemy)
    (define new-enemy-hps (- (unit-hps enemy) (unit-attack attacker)))
    (if (> new-enemy-hps 0)
        (begin
          (printf "Hi Edward c, Updating hps for enemy:~a at coor:~a to:~a~n" enemy enemy-coor new-enemy-hps)
          (hash-set! units enemy-coor (struct-copy unit (hash-ref units enemy-coor) (hps new-enemy-hps))))
        (begin
          (hash-remove! units enemy-coor)
          (printf "Hi Edward cc, removing enemy at ~a~n" enemy-coor))
        ))))

(define (get-total-hps units)
  (apply + (map (lambda (coor) (unit-hps (hash-ref units coor))) (hash-keys units))))

(define (part-one filename)
  (define-values (cave units) (load-cave filename))
  (define prev-hps (get-total-hps units))
  (define prev-rnd 0)
  (printf "~nInitial round:~n")
  (draw-cave cave units)
  (let loop ((rnd 0))
   (printf "Units A rnd:~a, prev-rnd:~a, hps: ~a~n" rnd prev-rnd (map (lambda (coor) (unit-hps (hash-ref units coor))) (sort (hash-keys units) reading-order)))
   (set! prev-hps (get-total-hps units))

   (for ((unit-coor (sort (hash-keys units) reading-order)))
        (printf "Units B rnd:~a, prev-rnd:~a, hps: ~a~n" rnd prev-rnd (map (lambda (coor) (unit-hps (hash-ref units coor))) (sort (hash-keys units) reading-order)))
        (when (hash-has-key? units unit-coor)
          (define unit (hash-ref units unit-coor))
          (define enemy (if (eq? (unit-type unit) 'elf) 'goblin 'elf))
          (define enemies-in-range (enemy-in-range units unit-coor enemy))
          (if enemies-in-range
              (combat units unit-coor enemies-in-range)
              (when (> (length (get-units units enemy)) 0)
                (define the-move (get-unit-move unit-coor cave units))
                (printf "Hi Edward 8, moving ~a, ~a to ~a~n" unit-coor unit the-move)
                (when the-move
                  (printf "Hi Edward 9, units:~a~n" units)
                  (hash-set! units the-move (hash-ref units unit-coor))
                  (hash-remove! units unit-coor))))))

   (printf "~nAfter ~a round:~n" (add1 rnd))
   (draw-cave cave units)
   (printf "Units C rnd:~a, prev-rnd:~a, hps: ~a~n" rnd prev-rnd (map (lambda (coor) (unit-hps (hash-ref units coor))) (sort (hash-keys units) reading-order)))
   (printf "Total prev-hps:~a, round:~a~n" prev-hps rnd)
   (set! prev-rnd rnd)
   (when (and (not (zero? (length (get-units units 'goblin))))
              (not (zero? (length (get-units units 'elf)))))
     (loop (add1 rnd))))
  (* prev-hps prev-rnd))

; 202650 too high

; python day15.py
; * part 1: 195774
; * part 2: 37272

; --- Part Two ---
;
; According to your calculations, the Elves are going to lose badly. Surely,
;  you won't mess up the timeline too much if you give them just a little
;  advanced technology, right?
;
; You need to make sure the Elves not only win, but also suffer no losses:
;  even the death of a single Elf is unacceptable.
;
; However, you can't go too far: larger changes will be more likely to
;  permanently alter spacetime.
;
; So, you need to find the outcome of the battle in which the Elves have the
;  lowest integer attack power (at least 4) that allows them to win without
;  a single death. The Goblins always have an attack power of 3.
;
; In the first summarized example above, the lowest attack power the Elves
;  need to win without losses is 15:
;
; #######       #######
; #.G...#       #..E..#   E(158)
; #...EG#       #...E.#   E(14)
; #.#.#G#  -->  #.#.#.#
; #..G#E#       #...#.#
; #.....#       #.....#
; #######       #######
;
; Combat ends after 29 full rounds
; Elves win with 172 total hit points left
; Outcome: 29 * 172 = 4988
;
; In the second example above, the Elves need only 4 attack power:
;
; #######       #######
; #E..EG#       #.E.E.#   E(200), E(23)
; #.#G.E#       #.#E..#   E(200)
; #E.##E#  -->  #E.##E#   E(125), E(200)
; #G..#.#       #.E.#.#   E(200)
; #..E#.#       #...#.#
; #######       #######
;
; Combat ends after 33 full rounds
; Elves win with 948 total hit points left
; Outcome: 33 * 948 = 31284
;
; In the third example above, the Elves need 15 attack power:
;
; #######       #######
; #E.G#.#       #.E.#.#   E(8)
; #.#G..#       #.#E..#   E(86)
; #G.#.G#  -->  #..#..#
; #G..#.#       #...#.#
; #...E.#       #.....#
; #######       #######
;
; Combat ends after 37 full rounds
; Elves win with 94 total hit points left
; Outcome: 37 * 94 = 3478
;
; In the fourth example above, the Elves need 12 attack power:
;
; #######       #######
; #.E...#       #...E.#   E(14)
; #.#..G#       #.#..E#   E(152)
; #.###.#  -->  #.###.#
; #E#G#G#       #.#.#.#
; #...#G#       #...#.#
; #######       #######
;
; Combat ends after 39 full rounds
; Elves win with 166 total hit points left
; Outcome: 39 * 166 = 6474
;
; In the last example above, the lone Elf needs 34 attack power:
;
; #########       #########   
; #G......#       #.......#   
; #.E.#...#       #.E.#...#   E(38)
; #..##..G#       #..##...#   
; #...##..#  -->  #...##..#   
; #...#...#       #...#...#   
; #.G...G.#       #.......#   
; #.....G.#       #.......#   
; #########       #########   
;
; Combat ends after 30 full rounds
; Elves win with 38 total hit points left
; Outcome: 30 * 38 = 1140
;
; After increasing the Elves' attack power until it is just barely enough
;  for them to win without any Elves dying, what is the outcome of the
;  combat described in your puzzle input? (Answer: ?)
;

; python day15.py
; * part 1: 195774
; * part 2: 37272

