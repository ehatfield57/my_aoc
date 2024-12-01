
(require rackunit)
(require json)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 12: JSAbacusFramework.io ---
;
; Santa's Accounting-Elves need help balancing the books after a recent order.
;  Unfortunately, their accounting software uses a peculiar storage format.
;  That's where you come in.
;
; They have a JSON document which contains a variety of things: arrays ([1,2,3]),
;  objects ({"a":1, "b":2}), numbers, and strings. Your first job is to simply find
;  all of the numbers throughout the document and add them together.
;
; For example:
;
; * [1,2,3] and {"a":2,"b":4} both have a sum of 6.
;
; * [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
;
; * {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
;
; * [] and {} both have a sum of 0.
;
; You will not encounter any strings containing numbers.
;
; What is the sum of all numbers in the document?  (Answer: 119433)
;

(define (part-one filename)
  (let* ((in (open-input-file filename))
         (foo (read-line in)))
    (close-input-port in)
    (apply + (map string->number (regexp-match* #rx"(-?[0-9]+)" foo)))))

(check-equal? (part-one "test-data-1.txt") 18)

(check-equal? (part-one "day12-data.txt") 119433)


;
; --- Part Two ---
;
; Uh oh - the Accounting-Elves have realized that they double-counted everything red.
;
; Ignore any object (and all of its children) which has any property with the value "red".
;  Do this only for objects ({...}), not arrays ([...]).
;
; * [1,2,3] still has a sum of 6.
;
; * [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.
;
; * {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire structure is ignored.
;
; * [1,"red",5] has a sum of 6, because "red" in an array has no effect.
;
; Answer: 
;

(define total 0)

(define (traverse thing (depth 0))
  (cond
    ((hash? thing)
     (for/list ((k (hash-keys thing)))
               (unless (member "red" (hash-values thing))
                 (traverse (hash-ref thing k) (add1 depth)))))
    ((list? thing) (for/list ((t thing)) (traverse t (add1 depth))))
    ((string? thing) #f)
    ((number? thing)
     (set! total (+ thing total)))
    (else (error (printf "Not sure: '~a'~n" thing)))))

(define (part-two filename)
  (let* ((in (open-input-file filename))
         (bar (read-json in)))
    (close-input-port in)
    (set! total 0)
    (traverse bar)))

;(define bar (part-two "test-data-2.txt"))
(define bar (part-two "day12-data.txt"))

(check-equal? total 68466)

