
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 19: An Elephant Named Joseph ---
;
; The Elves contact you over a highly secure emergency channel. Back at the
;  North Pole, the Elves are busy misunderstanding White Elephant parties.
;
; Each Elf brings a present. They all sit in a circle, numbered starting with
;  position 1. Then, starting with the first Elf, they take turns stealing all
;  the presents from the Elf to their left. An Elf with no presents is removed
;  from the circle and does not take turns.
;
; For example, with five Elves (numbered 1 to 5):
;
;   1
; 5   2
;  4 3
;
; * Elf 1 takes Elf 2's present.
; * Elf 2 has no presents and is skipped.
; * Elf 3 takes Elf 4's present.
; * Elf 4 has no presents and is also skipped.
; * Elf 5 takes Elf 1's two presents.
; * Neither Elf 1 nor Elf 2 have any presents, so both are skipped.
; * Elf 3 takes Elf 5's three presents.
;
; So, with five Elves, the Elf that sits starting in position 3 gets all the presents.
;
; With the number of Elves given in your puzzle input, which Elf gets all the presents?
;
; Your puzzle input is 3001330. (Answer: 1808357)
;

(define (party num)
  (let* ((table (make-vector num 1))
         (t-idx (lambda (n) (modulo (sub1 n) num))))
    (let loop1 ((idx 1))
     (when (> (vector-ref table (t-idx idx)) 0)
       (let loop2 ((sub-idx 1))
        (when (< sub-idx num)
          (if (zero? (vector-ref table (t-idx (+ sub-idx (t-idx idx) 1))))
              (loop2 (add1 sub-idx))
              (let* ((a (t-idx idx))
                     (b (t-idx (+ a sub-idx 1))))
                (vector-set! table a (+ (vector-ref table a) (vector-ref table b)))
                (vector-set! table b 0))))))
     (if (= num (vector-ref table (t-idx idx)))
         (printf "~a -> ~a~n" num (add1 (t-idx idx)))
         (loop1 (add1 idx))))))

(define (part-one limit)
  (for ((i (range 5 (add1 limit))))
       (party i)))

;
; --- Part Two ---
;
; Realizing the folly of their present-exchange rules, the Elves agree to instead
;  steal presents from the Elf directly across the circle. If two Elves are across
;  the circle, the one on the left (from the perspective of the stealer) is stolen
;  from. The other rules remain unchanged: Elves with no presents are removed from
;  the circle entirely, and the other elves move in slightly to keep the circle
;  evenly spaced.
;
; For example, with five Elves (again numbered 1 to 5):
;
; * The Elves sit in a circle; Elf 1 goes first:
;
;   1
; 5   2
;  4 3
;
; * Elves 3 and 4 are across the circle; Elf 3's present is stolen, being the one to
;    the left. Elf 3 leaves the circle, and the rest of the Elves move in:
;
;   1           1
; 5   2  -->  5   2
;  4 -          4
;
; * Elf 2 steals from the Elf directly across the circle, Elf 5:
;
;   1         1 
; -   2  -->     2
;   4         4 
;
; * Next is Elf 4 who, choosing between Elves 1 and 2, steals from Elf 1:
;
;  -          2  
;     2  -->
;  4          4
;
; * Finally, Elf 2 steals from Elf 4:
;
;  2
;     -->  2  
;  -
;
; So, with five Elves, the Elf that sits starting in position 2 gets all the
;  presents.
;
; With the number of Elves given in your puzzle input, which Elf now gets all
;  the presents? (Answer: 1407007)
;

(define (find-index element lst (acc 0))
  (cond
    ((empty? lst) #f)
    ((equal? element (car lst)) acc)
    (else (find-index element (cdr lst) (add1 acc)))))

(define (remove-middle data num)
  (printf "Hi Edward a, entering remove-middle with data: ~a and num: ~a~n" data num)
  (let ((offset (find-index num data)))
   (if (false? offset)
       data
       (let* ((len (length data))
              (post (modulo (add1 offset) len))
              (pre  (modulo (sub1 offset) len)))
         (when (< pre 0) (set! pre (+ pre len)))
         (do ((i 0 (add1 i)))
             ((or (= pre post) (= (add1 post) pre))(remove (list-ref data post) data))
             (set! post (modulo (add1 post) len))
             (set! pre  (modulo (sub1 pre)  len)))))))

(define (part-two elves)
  (let ((data (range 1 (add1 elves))))
   (printf "Hi Edward A, elves: ~a, data: ~a~n" elves data)
   (let loop ((numb 1))
    (printf "Hi Edward B, numb: ~a, data: ~a~n" numb data)
    (if (= 2 (length data))
      (set! data (remove-middle data 1))
      (set! data (remove-middle data numb)))
    (printf "Hi Edward C, data: ~a~n" data)
    (if (> (length data) 1)
      (loop (add1 numb))
      data))))
;
; Ultimately cheated and used the haskell code from Reddit. :-(
;
