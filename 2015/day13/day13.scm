
(require rackunit)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 13: Knights of the Dinner Table ---
;
; In years past, the holiday feast with your family hasn't gone so well. Not everyone
;  gets along! This year, you resolve, will be different. You're going to find the
;  optimal seating arrangement and avoid all those awkward conversations.
; 
; You start by writing up a list of everyone invited and the amount their happiness
;  would increase or decrease if they were to find themselves sitting next to each
;  other person. You have a circular table that will be just big enough to fit everyone
;  comfortably, and so each person will have exactly two neighbors.
; 
; For example, suppose you have only four attendees planned, and you calculate
;  their potential happiness as follows:
; 
; Alice would gain 54 happiness units by sitting next to Bob.
; Alice would lose 79 happiness units by sitting next to Carol.
; Alice would lose 2 happiness units by sitting next to David.
; Bob would gain 83 happiness units by sitting next to Alice.
; Bob would lose 7 happiness units by sitting next to Carol.
; Bob would lose 63 happiness units by sitting next to David.
; Carol would lose 62 happiness units by sitting next to Alice.
; Carol would gain 60 happiness units by sitting next to Bob.
; Carol would gain 55 happiness units by sitting next to David.
; David would gain 46 happiness units by sitting next to Alice.
; David would lose 7 happiness units by sitting next to Bob.
; David would gain 41 happiness units by sitting next to Carol.
;
; Then, if you seat Alice next to David, Alice would lose 2 happiness units (because
;  David talks so much), but David would gain 46 happiness units (because Alice is
;  such a good listener), for a total change of 44.
; 
; If you continue around the table, you could then seat Bob next to Alice (Bob gains 83,
;  Alice gains 54). Finally, seat Carol, who sits next to Bob (Carol gains 60, Bob loses
;  7) and David (Carol gains 55, David gains 41). The arrangement looks like this:
; 
;      +41 +46
; +55   David    -2
; Carol       Alice
; +60    Bob    +54
;      -7  +83
;
; After trying every other seating arrangement in this hypothetical scenario, you find
;  that this one is the most optimal, with a total change in happiness of 330.
; 
; What is the total change in happiness for the optimal seating arrangement of the
;  actual guest list? (Answer: 709)
;

(define (get-info filename)
  (let ((info (make-hash)))
   (call-with-input-file
     filename
     (lambda (in-port)
       (do ((line (read-line in-port 'any)
                  (read-line in-port 'any)))
           ((eof-object? line))
           (let ((parts (regexp-match* #rx"^(.*?) would (lose|gain) ([0-9]+) .*? ([^ ]+).$" line #:match-select values)))
             (define-values (from gainlose amt to) (apply values (cdr (car parts))))
             (define true-amt (if (string=? gainlose "gain") (string->number amt) (- (string->number amt))))
             (if (hash-has-key? info from)
                 (hash-set! info from (cons (cons to true-amt) (hash-ref info from)))
                 (hash-set! info from (list (cons to true-amt))))))))
   info))

(define (add-first-to-end group)
  (reverse (cons (car group) (reverse group))))

(define (iterate-pairs lst)
  (cond ((or (null? lst) (null? (cdr lst))) '())
        (else (cons (list (car lst) (cadr lst)) (iterate-pairs (cdr lst))))))

(define (extract-amount data a-pair)
  (let* ((first-person (car a-pair))
         (second-person (cadr a-pair))
         (neighbor-list (hash-ref data first-person))
         (second-list (assoc second-person neighbor-list)))
    (cdr second-list)))

(define (sum-group group data)
  (let ((my-pairs (iterate-pairs (add-first-to-end group))))
   (apply + (map (lambda (a-pair) (extract-amount data a-pair)) my-pairs))))

(define (arrangements data)
  (let* ((people (hash-keys data))
         (groupings (permutations people)))
    (apply max
           (map (lambda (group)
                  (+ (sum-group group data)
                     (sum-group (reverse group) data))
                  ) groupings))))

(define (part-one filename) (arrangements (get-info filename)))

(check-equal? (part-one "test-data.txt") 330)
(check-equal? (part-one "day13-data.txt") 709)

;
; --- Part Two ---
;
; In all the commotion, you realize that you forgot to seat yourself. At this
;  point, you're pretty apathetic toward the whole thing, and your happiness
;  wouldn't really go up or down regardless of who you sit next to. You assume
;  everyone else would be just as ambivalent about sitting next to you, too.
; 
; So, add yourself to the list, and give all happiness relationships that
;  involve you a score of 0.
; 
; What is the total change in happiness for the optimal seating arrangement
;  that actually includes yourself?  (Answer: 668)

(define (add-myself data)
  (let* ((people (hash-keys data)))
   (for-each (lambda (k) (hash-set! data k (cons (cons 'Edward 0) (hash-ref data k)))) people)
   (hash-set! data 'Edward '())
   (for-each (lambda (k) (hash-set! data 'Edward (cons (cons k 0) (hash-ref data 'Edward)))) people)
   data))

(define (part-two filename) (arrangements (add-myself (get-info filename))))

(check-equal? (part-two "test-data.txt") 286)
(check-equal? (part-two "day13-data.txt") 668)

