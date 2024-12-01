
(require rackunit)
(require racket/generator)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 15: Science for Hungry People ---
;
; Today, you set out on the task of perfecting your milk-dunking cookie recipe.
;  All you have to do is find the right balance of ingredients.
;
; Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a
;  list of the remaining ingredients you could use to finish the recipe (your
;  puzzle input) and their properties per teaspoon:
;
; * capacity (how well it helps the cookie absorb milk)
; * durability (how well it keeps the cookie intact when full of milk)
; * flavor (how tasty it makes the cookie)
; * texture (how it improves the feel of the cookie)
; * calories (how many calories it adds to the cookie)
;
; You can only measure ingredients in whole-teaspoon amounts accurately, and you
;  have to be accurate so you can reproduce your results in the future. The total
;  score of a cookie can be found by adding up each of the properties (negative
;  totals become 0) and then multiplying together everything except calories.
;
; For instance, suppose you have these two ingredients:
;
; Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
; Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
;
; Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon
;  (because the amounts of each ingredient must add up to 100) would result in a
;  cookie with the following properties:
;
; * A capacity of 44*-1 + 56*2 = 68
; * A durability of 44*-2 + 56*3 = 80
; * A flavor of 44*6 + 56*-2 = 152
; * A texture of 44*3 + 56*-1 = 76
;
; Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now) results
;  in a total score of 62842880, which happens to be the best score possible given
;  these ingredients. If any properties had produced a negative total, it would have
;  instead become zero, causing the whole score to multiply to zero.
;
; Given the ingredients in your kitchen and their properties, what is the total score
;  of the highest-scoring cookie you can make? (Answer: 222870)
;

(define teaspoons 100)

(struct ingredient (name capacity durability flavor texture calories) #:transparent)

(define (get-ingredients filename)
  (let ((ingredients '()))
   (call-with-input-file
     filename
     (lambda (in-port)
       (do ((line (read-line in-port 'any) (read-line in-port 'any)))
           ((eof-object? line))
           (let ((parts
                   (regexp-match*
                     #rx"^([^:]+):.*?(-?[0-9]+).*?(-?[0-9]+).*?(-?[0-9]+).*?(-?[0-9]+).*?(-?[0-9]+)"
                     line
                     #:match-select values)))
             (define-values (name capacity durability flavor texture calories) (apply values (cdr (car parts))))
             (set! ingredients
               (cons (ingredient name
                                 (string->number capacity)
                                 (string->number durability)
                                 (string->number flavor)
                                 (string->number texture)
                                 (string->number calories)) ingredients))))))
   (reverse ingredients)))

(define ingredients (get-ingredients "test-data.txt"))

(define percentages-2 (for*/list (
                                  [i (in-range 1 teaspoons)]
                                  [j (in-range 1 teaspoons)]
                                  #:when (= teaspoons (+ i j)))
                                 (list i j)))

(define percentages-4 (for*/list (
                                  [i (in-range 1 teaspoons)]
                                  [j (in-range 1 teaspoons)]
                                  [k (in-range 1 teaspoons)]
                                  [l (in-range 1 teaspoons)]
                                  #:when (= teaspoons (+ i j k l)))
                                 (list i j k l)))

(define percentages '(44 56))

(define i-fields
  (list ingredient-capacity
        ingredient-durability
        ingredient-flavor
        ingredient-texture))

(define (field-ingredients ingredients field-ref)
  (map field-ref ingredients))

(define (ing-formula percentages ing-fields)
  (apply + (map (lambda (pct ing) (* pct ing)) percentages ing-fields)))

(check-equal? (ing-formula percentages (field-ingredients ingredients ingredient-capacity)) 68)
(check-equal? (ing-formula percentages (field-ingredients ingredients ingredient-durability)) 80)
(check-equal? (ing-formula percentages (field-ingredients ingredients ingredient-flavor)) 152)
(check-equal? (ing-formula percentages (field-ingredients ingredients ingredient-texture)) 76)

(define (combine-ing i-fields percentages ingredients)
  (apply *
         (map (lambda (n) (if (< n 0) 0 n))
                 (map (lambda (i)
                        (ing-formula percentages (field-ingredients ingredients i))) i-fields))))


(check-equal? (combine-ing i-fields percentages ingredients) 62842880)

(define (find-best-ratios filename percentages (skip-calories #t))
  (let ((max-pct '())
        (max-amt   0)
        (ingredients (get-ingredients filename))
        (calorie-check (lambda (c) (if (eqv? #f skip-calories) (= c 500) #t))))
    (for ((percentage percentages))
         (let ((amount (combine-ing i-fields percentage ingredients))
               (calories (combine-ing (list ingredient-calories) percentage ingredients)))
           (when (and (calorie-check calories) (> amount max-amt))
             (set! max-amt amount)
             (set! max-pct percentage))))
    (printf "With data, the max amount is ~a with percentages ~a~n" max-amt max-pct)
    (values max-amt max-pct)))

(define-values (amt pcts) (find-best-ratios "test-data.txt" percentages-2))

(check-equal? amt 62842880)
(check-equal? pcts '(44 56))

(define-values (amt pcts) (find-best-ratios "day15-data.txt" percentages-4))

(check-equal? amt 222870)
(check-equal? pcts '(21 5 31 43))

;
; --- Part Two ---
;
; Your cookie recipe becomes wildly popular! Someone asks if you can make
;  another recipe that has exactly 500 calories per cookie (so they can use
;  it as a meal replacement). Keep the rest of your award-winning process
;  the same (100 teaspoons, same ingredients, same scoring system).
;
; For example, given the ingredients above, if you had instead selected 40
;  teaspoons of butterscotch and 60 teaspoons of cinnamon (which still adds
;  to 100), the total calorie count would be 40*8 + 60*3 = 500. The total
;  score would go down, though: only 57600000, the best you can do in such
;  trying circumstances.
;
; Given the ingredients in your kitchen and their properties, what is the
;  total score of the highest-scoring cookie you can make with a calorie
;  total of 500?  (Answer: 117936)
;

(define-values (amt pcts) (find-best-ratios "test-data.txt" percentages-2 #f))
(check-equal? amt 57600000)
(check-equal? pcts '(40 60))

(define-values (amt pcts) (find-best-ratios "day15-data.txt" percentages-4 #f))
(check-equal? amt 117936)
(check-equal? pcts '(21 8 26 45))

