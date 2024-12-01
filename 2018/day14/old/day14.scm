
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 14: Chocolate Charts ---
;
; You finally have a chance to look at all of the produce moving around.
;  Chocolate, cinnamon, mint, chili peppers, nutmeg, vanilla... the Elves
;  must be growing these plants to make hot chocolate! As you realize this,
;  you hear a conversation in the distance. When you go to investigate,
;  you discover two Elves in what appears to be a makeshift underground
;  kitchen/laboratory.
;
; The Elves are trying to come up with the ultimate hot chocolate recipe;
;  they're even maintaining a scoreboard which tracks the quality score
;  (0-9) of each recipe.
;
; Only two recipes are on the board: the first recipe got a score of 3, the
;  second, 7. Each of the two Elves has a current recipe: the first Elf starts
;  with the first recipe, and the second Elf starts with the second recipe.
;
; To create new recipes, the two Elves combine their current recipes. This
;  creates new recipes from the digits of the sum of the current recipes'
;  scores. With the current recipes' scores of 3 and 7, their sum is 10,
;  and so two new recipes would be created: the first with score 1 and the
;  second with score 0. If the current recipes' scores were 2 and 3, the sum,
;  5, would only create one recipe (with a score of 5) with its single digit.
;
; The new recipes are added to the end of the scoreboard in the order they are
;  created. So, after the first round, the scoreboard is 3, 7, 1, 0.
;
; After all new recipes are added to the scoreboard, each Elf picks a new current
;  recipe. To do this, the Elf steps forward through the scoreboard a number of
;  recipes equal to 1 plus the score of their current recipe. So, after the first
;  round, the first Elf moves forward 1 + 3 = 4 times, while the second Elf moves
;  forward 1 + 7 = 8 times. If they run out of recipes, they loop back around to
;  the beginning. After the first round, both Elves happen to loop around until
;  they land on the same recipe that they had in the beginning; in general, they
;  will move to different recipes.
;
; Drawing the first Elf as parentheses and the second Elf as square brackets, they
;  continue this process:
;
; (3)[7]
; (3)[7] 1  0 
;  3  7  1 [0](1) 0 
;  3  7  1  0 [1] 0 (1)
; (3) 7  1  0  1  0 [1] 2 
;  3  7  1  0 (1) 0  1  2 [4]
;  3  7  1 [0] 1  0 (1) 2  4  5 
;  3  7  1  0 [1] 0  1  2 (4) 5  1 
;  3 (7) 1  0  1  0 [1] 2  4  5  1  5 
;  3  7  1  0  1  0  1  2 [4](5) 1  5  8 
;  3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
;  3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6 
;  3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7 
;  3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7 
;  3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9 
;  3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2 
;
; The Elves think their skill will improve after making a few recipes (your puzzle
;  input). However, that could take ages; you can speed this up considerably by
;  identifying the scores of the ten recipes after that. For example:
;
; * If the Elves think their skill will improve after making 9 recipes, the scores of
;    the ten recipes after the first nine on the scoreboard would be 5158916779 (highlighted
;    in the last line of the diagram).
; * After 5 recipes, the scores of the next ten would be 0124515891.
; * After 18 recipes, the scores of the next ten would be 9251071085.
; * After 2018 recipes, the scores of the next ten would be 5941429882.
;
; What are the scores of the ten recipes immediately after the number of recipes
;  in your puzzle input? (Answer: 8610321414)
;
; Your puzzle input is 607331.
;

(define (show-info elves recipes)
  (define elf-xref (foldl (lambda (e acc) (hash-set! acc (hash-ref elves e) e) acc) (make-hash) (hash-keys elves)))
  (for ((r (hash-keys recipes))) (printf "~a~a " (if (hash-has-key? elf-xref r) "." " ") (hash-ref recipes r)))
  (printf "~n"))

(define (setup-data p-input)
  (let ((elves (make-hash))
        (recipes (make-hash)))
    (for ((n (string->list (number->string p-input)))
          (i (range (string-length (number->string p-input)))))
         (hash-set! recipes i (string->number (string n)))
         (hash-set! elves i i))
    (values elves recipes)))

(define (make-recipes elves recipes)
  (define recipe-cnt 0)

  ; Combine each elves current number to make new recipe
  (define new-recipe (foldl (lambda (n acc) (+ acc (hash-ref recipes (hash-ref elves n)))) 0 (hash-keys elves)))
  (set! recipe-cnt (length (hash-keys recipes)))

  ; Add to recipes hash
  (for ((n (string->list (number->string new-recipe)))
        (i (range recipe-cnt (+ recipe-cnt (string-length (number->string new-recipe))))))
       (hash-set! recipes i (string->number (string n))))

  ; Have the elves skip forward the amount of their recipe
  (set! recipe-cnt (length (hash-keys recipes)))
  (for ((e (hash-keys elves)))
       (let* ((elf-current-idx (hash-ref elves e))
              (elf-ptr-number  (hash-ref recipes elf-current-idx)))
         (hash-set! elves e (modulo (+ 1 elf-current-idx elf-ptr-number) recipe-cnt))))

  (apply string-append
         (map number->string
              (foldl (lambda (k acc) (append (list (hash-ref recipes k)) acc)) '()
                     (sort (hash-keys recipes) >)))))

(define (part-one p-input cycles)
  (define recipe-str "")
  (define-values (elves recipes) (setup-data p-input))

  (for ((t (range (+ 10 (- cycles (string-length (number->string p-input)))))))
       (set! recipe-str (make-recipes elves recipes))
       (when (zero? (modulo t 1000)) (printf "~a~n" t)))

  (substring recipe-str cycles (+ cycles 10)))

;(check-equal? (part-one 37 9) "5158916779")
;(check-equal? (part-one 37 5) "0124515891")
;(check-equal? (part-one 37 18) "9251071085")
;(check-equal? (part-one 37 2018) "5941429882")

(check-equal? (part-one 37 607331) "8610321414") ; My code is too slow. :-(

;
; --- Part Two ---
;
; As it turns out, you got the Elves' plan backwards. They actually want
;  to know how many recipes appear on the scoreboard to the left of the
;  first recipes whose scores are the digits from your puzzle input.
;
; 51589 first appears after 9 recipes.
; 01245 first appears after 5 recipes.
; 92510 first appears after 18 recipes.
; 59414 first appears after 2018 recipes.
;
; How many recipes appear on the scoreboard to the left of the score
;  sequence in your puzzle input? (Answer: 20258123)
;

(define (part-two p-input goal-str)
  (define recipe-str "")
  (define str-index 0)
  (define-values (elves recipes) (setup-data p-input))

  (let loop ((c 0))
   (set! recipe-str (make-recipes elves recipes))
   (if (string-contains? recipe-str goal-str)
       (set! str-index (- (string-length recipe-str) (string-length goal-str)))
       (loop (add1 c)))
   str-index))

;(check-equal? (part-two 37 "51589") 9)
;(check-equal? (part-two 37 "01245") 5)
;(check-equal? (part-two 37 "92510") 18)
;(check-equal? (part-two 37 "59414") 2018)

;(check-equal? (part-two 607331 "86103") 20258123)

