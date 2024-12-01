
(require rackunit)
(require srfi/1) ; find
(require racket/trace)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 22: Wizard Simulator 20XX ---
;
; Little Henry Case decides that defeating bosses with swords and stuff is
;  boring. Now he's playing the game with a wizard. Of course, he gets stuck
;  on another boss and needs your help again.
;
; In this version, combat still proceeds with the player and the boss taking
;  alternating turns. The player still goes first. Now, however, you don't
;  get any equipment; instead, you must choose one of your spells to cast.
;  The first character at or below 0 hit points loses.
;
; Since you're a wizard, you don't get to wear armor, and you can't attack
;  normally. However, since you do magic damage, your opponent's armor is
;  ignored, and so the boss effectively has zero armor as well. As before,
;  if armor (from a spell, in this case) would reduce damage below 1, it
;  becomes 1 instead - that is, the boss' attacks always deal at least 1 damage.
;
; On each of your turns, you must select one of your spells to cast. If you
;  cannot afford to cast any spell, you lose. Spells cost mana; you start with
;  500 mana, but have no maximum limit. You must have enough mana to cast a spell,
;  and its cost is immediately deducted when you cast it. Your spells are Magic
;  Missile, Drain, Shield, Poison, and Recharge.
;
; * Magic Missile costs 53 mana. It instantly does 4 damage.
;
; * Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
;
; * Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it
;    is active, your armor is increased by 7.
;
; * Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start
;    of each turn while it is active, it deals the boss 3 damage.
;
; * Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the
;    start of each turn while it is active, it gives you 101 new mana.
;
; * Effects all work the same way. Effects apply at the start of both the player's
;    turns and the boss' turns. Effects are created with a timer (the number of
;    turns they last); at the start of each turn, after they apply any effect they
;    have, their timer is decreased by one. If this decreases the timer to zero,
;    the effect ends. You cannot cast a spell that would start an effect which is
;    already active. However, effects can be started on the same turn they end.
;
; For example, suppose the player has 10 hit points and 250 mana, and that the boss
;  has 13 hit points and 8 damage:
;
; -- Player turn --
; - Player has 10 hit points, 0 armor, 250 mana
; - Boss has 13 hit points
; Player casts Poison.
;
; -- Boss turn --
; - Player has 10 hit points, 0 armor, 77 mana
; - Boss has 13 hit points
; Poison deals 3 damage; its timer is now 5.
; Boss attacks for 8 damage.
;
; -- Player turn --
; - Player has 2 hit points, 0 armor, 77 mana
; - Boss has 10 hit points
; Poison deals 3 damage; its timer is now 4.
; Player casts Magic Missile, dealing 4 damage.
;
; -- Boss turn --
; - Player has 2 hit points, 0 armor, 24 mana
; - Boss has 3 hit points
; Poison deals 3 damage. This kills the boss, and the player wins.
; Now, suppose the same initial conditions, except that the boss has 14 hit points instead:
;
; -- Player turn --
; - Player has 10 hit points, 0 armor, 250 mana
; - Boss has 14 hit points
; Player casts Recharge.
;
; -- Boss turn --
; - Player has 10 hit points, 0 armor, 21 mana
; - Boss has 14 hit points
; Recharge provides 101 mana; its timer is now 4.
; Boss attacks for 8 damage!
;
; -- Player turn --
; - Player has 2 hit points, 0 armor, 122 mana
; - Boss has 14 hit points
; Recharge provides 101 mana; its timer is now 3.
; Player casts Shield, increasing armor by 7.
;
; -- Boss turn --
; - Player has 2 hit points, 7 armor, 110 mana
; - Boss has 14 hit points
; Shield's timer is now 5.
; Recharge provides 101 mana; its timer is now 2.
; Boss attacks for 8 - 7 = 1 damage!
;
; -- Player turn --
; - Player has 1 hit point, 7 armor, 211 mana
; - Boss has 14 hit points
; Shield's timer is now 4.
; Recharge provides 101 mana; its timer is now 1.
; Player casts Drain, dealing 2 damage, and healing 2 hit points.
;
; -- Boss turn --
; - Player has 3 hit points, 7 armor, 239 mana
; - Boss has 12 hit points
; Shield's timer is now 3.
; Recharge provides 101 mana; its timer is now 0.
; Recharge wears off.
; Boss attacks for 8 - 7 = 1 damage!
;
; -- Player turn --
; - Player has 2 hit points, 7 armor, 340 mana
; - Boss has 12 hit points
; Shield's timer is now 2.
; Player casts Poison.
;
; -- Boss turn --
; - Player has 2 hit points, 7 armor, 167 mana
; - Boss has 12 hit points
; Shield's timer is now 1.
; Poison deals 3 damage; its timer is now 5.
; Boss attacks for 8 - 7 = 1 damage!
;
; -- Player turn --
; - Player has 1 hit point, 7 armor, 167 mana
; - Boss has 9 hit points
; Shield's timer is now 0.
; Shield wears off, decreasing armor by 7.
; Poison deals 3 damage; its timer is now 4.
; Player casts Magic Missile, dealing 4 damage.
;
; -- Boss turn --
; - Player has 1 hit point, 0 armor, 114 mana
; - Boss has 2 hit points
; Poison deals 3 damage. This kills the boss, and the player wins.
;
; You start with 50 hit points and 500 mana points. The boss's actual
;  stats are in your puzzle input. What is the least amount of mana you
;  can spend and still win the fight? (Do not include mana recharge
;  effects as "spending" negative mana.)   (Answer: ?)
;

(define mana-spent 0)

(struct spell (name cost) #:transparent)

(define spells
  (list (spell 'magic-missile   53)
        (spell 'drain          73)
        (spell 'shield        113)
        (spell 'poison        173)
        (spell 'recharge      229)))

(define cheapest-spell 53)

(define (get-spell the-spell-name)
  (find (lambda (s) (eqv? the-spell-name (spell-name s))) spells))

(struct player (name hit-points damage armor mana effects) #:transparent #:mutable)

(define (in-effect player-a) (map car (player-effects player-a)))

(define (is-member? x lst) (not (not (member x lst))))

(define (spell-duration player-a the-spell-name)
  (let ((spells (player-effects player-a)))
   (cdr (assoc the-spell-name spells))))

(define (start-effect player-a the-spell-name duration cost)
  (if (and (is-member? the-spell-name (in-effect player-a)) (> (spell-duration player-a the-spell-name) 1))
      (printf "===> ~a already has ~a spell in effect~n" (player-name player-a) the-spell-name)
      (begin
        (set-player-mana! player-a (- (player-mana player-a) cost))
        (set-player-effects! player-a (cons (cons the-spell-name duration) (player-effects player-a))))))

(define (effect-effects player-a player-b)
  (let ((old-effects (player-effects player-a))
        (new-effects '()))
    (for ((effect old-effects))
         (let ((spl (car effect))
               (dur (cdr effect)))
           (cond
             ((eqv? spl 'shield)
              (printf "Shields timer is now ~a.~n" (sub1 dur))
              (if (zero? (sub1 dur))
                  (set-player-armor! player-a 0)
                  (set-player-armor! player-a 7)))

             ((eqv? spl 'poison)
              (set-player-hit-points! player-b (- (player-hit-points player-b) 3))
              (printf "Poison deals ~a damage; its timer is now ~a.~n" 3 (sub1 dur)))

             ((eqv? spl 'recharge)
              (set-player-mana! player-a (+ 101 (player-mana player-a)))
              (printf "Recharge provides ~a mana; its timer is now ~a.~n" 101 (sub1 dur))))

           (if (> dur 1)
               (set! new-effects (cons (cons spl (sub1 dur)) new-effects))
               (printf "~a has worn off.~n" spl))))
    (set-player-effects! player-a new-effects)))

(define (cast the-spell-name player-a player-b)
  (let ((cost (spell-cost (get-spell the-spell-name))))

   (if (< (player-mana player-a) cost)
       (printf "==> Player doesn't have enough mana to cast ~a~n" the-spell-name)
       (begin
         (set! mana-spent (+ mana-spent cost))
         (cond
           ((eqv? the-spell-name 'magic-missile)
            (set-player-mana! player-a (- (player-mana player-a) cost))
            (set-player-hit-points! player-b (- (player-hit-points player-b) 4))
            (printf "Player casts ~a, dealing ~a damage.~n" the-spell-name 4))

           ((eqv? the-spell-name 'drain)
            (set-player-mana! player-a (- (player-mana player-a) cost))
            (set-player-hit-points! player-b (- (player-hit-points player-b) 2)) 
            (set-player-hit-points! player-a (+ (player-hit-points player-a) 2))
            (printf "Player casts ~a, dealing ~a damage, and healing ~a hit points.~n" the-spell-name 2 2))

           ((eqv? the-spell-name 'shield)
            (start-effect player-a the-spell-name 6 cost)
            (set-player-armor! player-a 7)
            (printf "Player casts ~a, increasing armor by ~a.~n" the-spell-name 7))

           ((eqv? the-spell-name 'poison)
            (start-effect player-a the-spell-name 6 cost)
            (printf "Player casts ~a.~n" the-spell-name))

           ((eqv? the-spell-name 'recharge)
            (start-effect player-a the-spell-name 5 cost)
            (printf "Player casts ~a.~n" the-spell-name)))))))

(define (attack player-a player-b)
  (let* ((raw-damage (- (player-damage player-a) (player-armor player-b)))
         (damage (if (<= raw-damage 0) 1 raw-damage))
         (new-hit-points (- (player-hit-points player-b) damage)))
    (set-player-hit-points! player-b new-hit-points)
    (printf "~a attacks for ~a damage.~n" (player-name player-a) damage)))

(define (player-turn player-a player-b the-spell-name)
  (effect-effects player-a player-b)
  (when (<= (player-hit-points player-a) 0)
    (error "Player loses due to zero hit points"))
  (when (< (player-mana player-a) cheapest-spell)
    (error "Player loses due to lack of mana"))
  (cast the-spell-name player-a player-b))

(define (boss-turn player-a player-b)
  (effect-effects player-b player-a)
  (when (<= (player-hit-points player-a) 0)
    (error "Boss loses due to zero hit points"))
  (attack player-a player-b))

(define (game-status the-player the-boss)
  (printf "- ~a has ~a hit points, ~a armor, ~a mana~n"
          (player-name the-player)
          (player-hit-points the-player)
          (player-armor the-player)
          (player-mana the-player)) 

  (printf "- ~a has ~a hit points~n"
          (player-name the-boss)
          (player-hit-points the-boss)))

(define (play-around player-a player-b the-spell-name)
  (let ((player-a-name (player-name player-a))
        (player-b-name (player-name player-b)))

    (printf "~n-- ~a turn --~n" player-a-name)
    (game-status player-a player-b)
    (player-turn player-a player-b the-spell-name)

    (printf "~n-- ~a turn --~n" player-b-name)
    (game-status player-a player-b)
    (boss-turn player-b player-a)))

(define (test-example player-a player-b spell-list)
  (set! mana-spent 0)
  (for ((s spell-list)
        #:unless (or (<= (player-hit-points player-a) 0) (<= (player-hit-points player-b) 0)))
       (play-around player-a player-b s)))

;(define test-player (player 'Player  10  0  0 250 '()))
;(define test-boss   (player 'Boss    13  8  0   0 '()))
;(test-example test-player test-boss '(poison magic-missile magic-missile))

;(define test-player (player 'Player  10  0  0 250 '()))
;(define test-boss   (player 'Boss    14  8  0   0 '()))
;(test-example test-player test-boss '(recharge shield drain poison magic-missile))

(define you  (player 'player  50 0 0 500 '()))
(define boss (player 'boss    51 9 0   0 '()))
;(test-example you boss '(recharge shield poison recharge drain shield poison drain magic-missile shield magic-missile magic-missile)) ; 1448 (too high)
;(test-example you boss '(recharge shield poison recharge magic-missile shield poison magic-missile magic-missile shield magic-missile magic-missile)) ; 1355 (too high)
; According to Reddit, python code, my answer should be 900. :-/

; Spells taken from python output:
(test-example you boss '(recharge poison shield magic-missile poison magic-missile magic-missile magic-missile))
(check-equal? mana-spent 900)

