
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 24: Electromagnetic Moat ---
;
; The CPU itself is a large, black building surrounded by a bottomless pit.
;  Enormous metal tubes extend outward from the side of the building at
;  regular intervals and descend down into the void. There's no way to cross,
;  but you need to get inside.
;
; No way, of course, other than building a bridge out of the magnetic components
;  strewn about nearby.
;
; Each component has two ports, one on each end. The ports come in all different
;  types, and only matching types can be connected. You take an inventory of
;  the components by their port types (your puzzle input). Each port is identified
;  by the number of pins it uses; more pins mean a stronger connection for your
;  bridge. A 3/7 component, for example, has a type-3 port on one side, and a
;  type-7 port on the other.
;
; Your side of the pit is metallic; a perfect surface to connect a magnetic,
;  zero-pin port. Because of this, the first port you use must be of type 0.
;  It doesn't matter what type of port you end with; your goal is just to
;  make the bridge as strong as possible.
;
; The strength of a bridge is the sum of the port types in each component.
;  For example, if your bridge is made of components 0/3, 3/7, and 7/4, your
;  bridge has a strength of 0+3 + 3+7 + 7+4 = 24.
;
; For example, suppose you had the following components:
;
; 0/2
; 2/2
; 2/3
; 3/4
; 3/5
; 0/1
; 10/1
; 9/10
;
; With them, you could make the following valid bridges:
;
; * 0/1
; * 0/1--10/1
; * 0/1--10/1--9/10
; * 0/2
; * 0/2--2/3
; * 0/2--2/3--3/4
; * 0/2--2/3--3/5
; * 0/2--2/2
; * 0/2--2/2--2/3
; * 0/2--2/2--2/3--3/4
; * 0/2--2/2--2/3--3/5
;
; (Note how, as shown by 10/1, order of ports within a component doesn't matter.
;  However, you may only use each port on a component once.)
;
; Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of
;  0+1 + 1+10 + 10+9 = 31.
;
; What is the strength of the strongest bridge you can make with the components
;  you have available? (Answer: 1695)
;

(define max-strength 0)
(define strongest-span '())
(define longest-span '())

(define (get-pieces filename)
  (let ((lines (file->lines filename))
        (pieces (make-hash)))
    (for ((line lines))
         (define-values (a b) (apply values (regexp-match* #rx"([0-9]+)" line)))
         (when (not (hash-has-key? pieces a)) (hash-set! pieces a '()))
         (when (not (hash-has-key? pieces b)) (hash-set! pieces b '()))
         (hash-set! pieces a (cons b (hash-ref pieces a)))
         (hash-set! pieces b (cons a (hash-ref pieces b))))
    pieces))

(define (calc-strength ports)
  (let* ((whole (string-join ports))
         (numbers (regexp-match* #rx"([0-9]+)" whole)))
    (apply + (map string->number numbers))))

(define (helper pieces used port-a)
  (when (> (calc-strength used) max-strength)
    (set! max-strength (calc-strength used))
    (set! strongest-span used))
  (when (> (length used) (length longest-span))
    (set! longest-span used)
    (printf "Longest span length:~a, strength:~a, '~a'~n" (length used) (calc-strength used) used))
  (for ((port-b (hash-ref pieces port-a)))
       (let* ((componet (string-append port-a "/" port-b))
              (componet-rev (string-append port-b "/" port-a)))
         (when (and (not (member componet used)) (not (member componet-rev used)))
;           (printf "Max-strength: ~a, strongest-span: ~a~n" max-strength (reverse strongest-span))
           (append (list componet) (helper pieces (cons componet used) port-b)))))
  max-strength)

(define (part-one filename)
  (set! max-strength 0)
  (set! strongest-span '())
  (helper (get-pieces filename) '() "0"))

;(check-equal? (part-one "test-data.txt") 31)
;(check-equal? (part-one "day24-data.txt") 1695) ; Interrupted and tried largest number so far

;
; --- Part Two ---
;
; The bridge you've built isn't long enough; you can't jump the rest of the way.
;
; In the example above, there are two longest bridges:
;
; 0/2--2/2--2/3--3/4
; 0/2--2/2--2/3--3/5
;
; Of them, the one which uses the 3/5 component is stronger; its strength is 0+2 + 2+2 + 2+3 + 3+5 = 19.
;
; What is the strength of the longest bridge you can make? If you can make multiple
;  bridges of the longest length, pick the strongest one. (Answer: 1673)
;

;(part-one "day24-data.txt") ; -> 1673 (Again, interrupted and tried largest)

