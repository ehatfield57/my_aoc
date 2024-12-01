
(require rackunit)
(require math/number-theory)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 20: Infinite Elves and Infinite Houses ---
;
; To keep the Elves busy, Santa has them deliver some presents by hand,
;  door-to-door. He sends them down a street with infinite houses numbered
;  sequentially: 1, 2, 3, 4, 5, and so on.
;
; Each Elf is assigned a number, too, and delivers presents to houses
;  based on that number:
;
; * The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5, ....
;
; * The second Elf (number 2) delivers presents to every second house: 2, 4, 6, 8, 10, ....
;
; * Elf number 3 delivers presents to every third house: 3, 6, 9, 12, 15, ....
;
; There are infinitely many Elves, numbered starting with 1. Each Elf delivers
;  presents equal to ten times his or her number at each house.
;
; So, the first nine houses on the street end up like this:
;
; House 1 got 10 presents.
; House 2 got 30 presents.
; House 3 got 40 presents.
; House 4 got 70 presents.
; House 5 got 60 presents.
; House 6 got 120 presents.
; House 7 got 80 presents.
; House 8 got 150 presents.
; House 9 got 130 presents.
;
; The first house gets 10 presents: it is visited only by Elf 1, which delivers
;  1 * 10 = 10 presents. The fourth house gets 70 presents, because it is
;  visited by Elves 1, 2, and 4, for a total of 10 + 20 + 40 = 70 presents.
;
; What is the lowest house number of the house to get at least as many presents
;  as the number in your puzzle input?  (Answer: 831600)
;
; Your puzzle input is 36000000.
;

; I'm using the faster math/number-theory divisors function - EJH
;(define (divisors n)
;  (let ((the-divs (list n))
;        (start (round (/ n 2))))
;   (do ((i start (sub1 i)))
;       ((zero? i) the-divs)
;        (when (= 0 (remainder n i))
;          (set! the-divs (cons i the-divs))))))

;(define (presents mult house)
;  (* mult (apply + (divisors house))))

(define (part-one)
  (for ((i 10000000))
       (let ((present-count (* 10 (apply + (divisors i)))))
        (when (>= present-count 36000000)
          (printf "House ~a got ~a presents.~n" i present-count)
          (error "Stop here, please.")))))

;(part-one) ; -> 831600

;
; --- Part Two ---
;
; The Elves decide they don't want to visit an infinite number of houses. Instead,
;  each Elf will stop after delivering presents to 50 houses. To make up for it,
;  they decide to deliver presents equal to eleven times their number at each house.
;
; With these changes, what is the new lowest house number of the house to get at
;  least as many presents as the number in your puzzle input? (Answer: 884520)
; 

;
; Ok, I'm rethinking the problem.  Each elf only delivers to 50 houses, which means that
;  when a given factor (elf) has been used 50 times it can't be used again.
;

(define (get-available-divisors used-factors i)
  (let ((avail '()))
   (for ((n (divisors i)))
             (when (not (hash-has-key? used-factors n)) (hash-set! used-factors n 0))
             (when (< (hash-ref used-factors n) 50) (set! avail (cons n avail))))
   avail))

(define (update-used-factors used-factors lst)
  (for ((f lst))
            (when (not (hash-has-key? used-factors f)) (hash-set! used-factors f 0))
            (hash-set! used-factors f (add1 (hash-ref used-factors f)))))

(define (part-two)
  (let ((used-factors (make-hash)))
   (for ((i 10000000))
        (let* ((divs (get-available-divisors used-factors i))
               (present-count (* 11 (apply + divs))))
          (update-used-factors used-factors divs)
          (when (>= present-count 36000000)
            (printf "House ~a got ~a presents, using divs: '~a'.~n" i present-count divs)
            (error "Stop here, please."))))))

; -> 884520

