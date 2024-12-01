
(require rackunit)
(require math/number-theory)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 21: RPG Simulator 20XX ---
;
; Little Henry Case got a new video game for Christmas. It's an RPG, and he's stuck on
;  a boss. He needs to know what equipment to buy at the shop. He hands you the controller.
;
; In this game, the player (you) and the enemy (the boss) take turns attacking. The player
;  always goes first. Each attack reduces the opponent's hit points by at least 1. The
;  first character at or below 0 hit points loses.
;
; Damage dealt by an attacker each turn is equal to the attacker's damage score minus the
;  defender's armor score. An attacker always does at least 1 damage. So, if the attacker
;  has a damage score of 8, and the defender has an armor score of 3, the defender loses 5
;  hit points. If the defender had an armor score of 300, the defender would still lose 1
;  hit point.
;
; Your damage score and armor score both start at zero. They can be increased by buying items
;  in exchange for gold. You start with no items and have as much gold as you need. Your total
;  damage or armor is equal to the sum of those stats from all of your items. You have 100 hit
;  points.
;
; Here is what the item shop is selling:
;
; Weapons:    Cost  Damage  Armor
; Dagger        8     4       0
; Shortsword   10     5       0
; Warhammer    25     6       0
; Longsword    40     7       0
; Greataxe     74     8       0
;
; Armor:      Cost  Damage  Armor
; Leather      13     0       1
; Chainmail    31     0       2
; Splintmail   53     0       3
; Bandedmail   75     0       4
; Platemail   102     0       5
;
; Rings:      Cost  Damage  Armor
; Damage +1    25     1       0
; Damage +2    50     2       0
; Damage +3   100     3       0
; Defense +1   20     0       1
; Defense +2   40     0       2
; Defense +3   80     0       3
;
; You must buy exactly one weapon; no dual-wielding. Armor is optional, but you can't use more
;  than one. You can buy 0-2 rings (at most one for each hand). You must use any items you buy.
;  The shop only has one of each item, so you can't buy, for example, two rings of Damage +3.
;
; For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that the boss has 12
;  hit points, 7 damage, and 2 armor:
;
; * The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
; * The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
; * The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
; * The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
; * The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
; * The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
; * The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.
;
; In this scenario, the player wins! (Barely.)
;
; You have 100 hit points. The boss's actual stats are in your puzzle input. What is the least
;  amount of gold you can spend and still win the fight?  (Answer: 78)
;

(struct item (type name cost damage armor) #:transparent)

(define place-holder (item 'nothing 'nothing 0  0  0))

(define items
  (list
    (item 'weapon 'dagger       8  4  0)
    (item 'weapon 'shortsword  10  5  0)
    (item 'weapon 'warhammer   25  6  0)
    (item 'weapon 'longsword   40  7  0)
    (item 'weapon 'greataxe    74  8  0)
    (item 'armor  'leather     13  0  1)
    (item 'armor  'chainmail   31  0  2)
    (item 'armor  'splintmail  53  0  3)
    (item 'armor  'bandedmail  75  0  4)
    (item 'armor  'platemail  102  0  5)
    (item 'ring   'damage+1    25  1  0)
    (item 'ring   'damage+2    50  2  0)
    (item 'ring   'damage+3   100  3  0)
    (item 'ring   'defense+1   20  0  1)
    (item 'ring   'defense+2   40  0  2)
    (item 'ring   'defense+3   80  0  3)))

(struct player (name hit-points damage armor) #:transparent #:mutable)

(define (attack player-a player-b)
  (let* ((raw-damage (- (player-damage player-a) (player-armor player-b)))
         (damage (if (<= raw-damage 0) 1 raw-damage))
         (new-hit-points (- (player-hit-points player-b) damage)))
;    (printf "The ~a deals ~a-~a = ~a damage; the ~a goes down to ~a hit points.~n"
;            (player-name player-a)
;            (player-damage player-a)
;            (player-armor player-b)
;            damage
;            (player-name player-b)
;            new-hit-points)
    (set-player-hit-points! player-b new-hit-points)))

(define (test-example player-a player-b)
  (do ((i 1 (add1 i)))
      ((or (<= (player-hit-points player-a) 0) (<= (player-hit-points player-b) 0)))
      (attack player-a player-b)
      (when (> (player-hit-points player-b) 0) (attack player-b player-a)))
;  (printf "~nIn this scenario, the ~a wins! (Barely.)~n"
;          (if (<= (player-hit-points player-a) 0) (player-name player-b) (player-name player-a)))
  )

(define test-player (player 'player   8  5  5))
(define test-boss   (player 'boss    12  7  2))
(test-example test-player test-boss)

(define you  (player 'player 100 0 0))
(define boss (player 'boss   104 8 1))

;
; Thoughts:
; 1. I could start with the cheapest things and then work my
;    way up in the permutations to find the lowest cost and win.
; 2. Combinations of:
;    a. 1 weapon (5)
;    b. 0 or 1 armor (6)  (no-ring + 6)
;    c. 0 or 2 rings (29) (no-ring + 28)
;
; Example:
;  Weapon     Armor     Ring(s)  Costs
;  =========  ========  =======  =====
;  dagger     none      none         8
;  dagger     none      damage+1    33 (25 + 8)
;  dagger     none      damage+2    58 (50 + 8)
;  ...
;  dagger     Platemail none       110 (110 + 8)
;  shortsword none      none        10
;  ...
;  greataxe   platemail dmg3,def3  356
;

(define (equip stuff)
  (let ((damage 0)
        (armor  0))
    (for ((i stuff))
         (set! damage (+ damage (item-damage i)))
         (set! armor  (+ armor  (item-armor  i))))
    (cons damage armor)))

(define (tally-gold stuff)
  (let ((gold 0))
   (for ((i stuff))
        (set! gold (+ gold (item-cost i))))
   gold))

(define (run-scenario my-items)
  (let* ((player-a (struct-copy player you))
         (player-b (struct-copy player boss))
         (stats   (equip my-items)))
    ;    (printf "Player equiped with: ~a, totally ~a gold.~n" my-items (tally-gold my-items))
    (set-player-damage! player-a (car stats))
    (set-player-armor!  player-a (cdr stats))
    (do ((i 1 (add1 i)))
        ((or (<= (player-hit-points player-a) 0) (<= (player-hit-points player-b) 0)))
        (attack player-a player-b)
        (when (> (player-hit-points player-b) 0) (attack player-b player-a)))
    (cons
      (tally-gold my-items)
      (if (> (player-hit-points player-a) 0) (player-name player-a) (player-name player-b)))))

(define (part-one)
  (let* ((the-weapons (filter (lambda (i) (eqv? (item-type i) 'weapon)) items))
         (all-armor   (filter (lambda (i) (eqv? (item-type i) 'armor))  items))
         (all-rings   (filter (lambda (i) (eqv? (item-type i) 'ring))   items))
         (the-armor   (cons place-holder all-armor))
         (the-rings   (cons place-holder (append all-rings (combinations all-rings 2)))))
    (for ((w the-weapons))
         (for ((a the-armor))
              (for ((r the-rings))
                   (let ((item-group (list w a)))
                    (if (and (list? r) (> (length r) 1))
                        (set! item-group (append r item-group))
                        (set! item-group (cons r item-group)))
                    (let ((results (run-scenario item-group)))
                     (printf "~a gold, items: ~a and winner: ~a~n"
                             (car results)
                             item-group
                             (cdr results)))))))))

;
; 78 gold,
; items: (
;   #(struct:item ring damage+1 25 1 0)
;   #(struct:item weapon longsword 40 7 0)
;   #(struct:item armor leather 13 0 1))
; and winner: player
;

;
; --- Part Two ---
;
; Turns out the shopkeeper is working with the boss, and can persuade you to buy
;  whatever items he wants. The other rules still apply, and he still only has
;  one of each item.
;
; What is the most amount of gold you can spend and still lose the fight? (Answer: 148)
;

;
; 148 gold,
; items: (
;   #(struct:item ring damage+3 100 3 0)
;   #(struct:item ring defense+2 40 0 2)
;   #(struct:item weapon dagger 8 4 0)
;   #(struct:item nothing nothing 0 0 0))
; and winner: boss
;

