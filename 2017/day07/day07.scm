
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 7: Recursive Circus ---
;
; Wandering further through the circuits of the computer, you come upon
;  a tower of programs that have gotten themselves into a bit of trouble.
;  A recursive algorithm has gotten out of hand, and now they're balanced
;  precariously in a large tower.
;
; One program at the bottom supports the entire tower. It's holding a large
;  disc, and on the disc are balanced several more sub-towers. At the bottom
;  of these sub-towers, standing on the bottom disc, are other programs, each
;  holding their own disc, and so on. At the very tops of these sub-sub-sub-...-towers,
;  many programs stand simply keeping the disc below them balanced but with no disc
;  of their own.
;
; You offer to help, but first you need to understand the structure of these towers.
;  You ask each program to yell out their name, their weight, and (if they're holding
;  a disc) the names of the programs immediately above them balancing on that disc.
;  You write this information down (your puzzle input). Unfortunately, in their panic,
;  they don't do this in an orderly fashion; by the time you're done, you're not sure
;  which program gave which information.
;
; For example, if your list is the following:
;
; pbga (66)
; xhth (57)
; ebii (61)
; havc (66)
; ktlj (57)
; fwft (72) -> ktlj, cntj, xhth
; qoyq (66)
; padx (45) -> pbga, havc, qoyq
; tknk (41) -> ugml, padx, fwft
; jptl (61)
; ugml (68) -> gyxo, ebii, jptl
; gyxo (61)
; cntj (57)
;
; ...then you would be able to recreate the structure of the towers that looks like this:
;
;                 gyxo
;               /     
;          ugml - ebii
;        /      \     
;       |         jptl
;       |        
;       |         pbga
;      /        /
; tknk --- padx - havc
;      \        \
;       |         qoyq
;       |             
;       |         ktlj
;        \      /     
;          fwft - cntj
;               \     
;                 xhth
;
; In this example, tknk is at the bottom of the tower (the bottom program), and is holding
;  up ugml, padx, and fwft. Those programs are, in turn, holding up other programs; in
;  this example, none of those programs are holding up any other programs, and are all the
;  tops of their own towers. (The actual tower balancing in front of you is much larger.)
;
; Before you're ready to help them, you need to make sure your information is correct. What
;  is the name of the bottom program? (Answer: qibuqqg)
;

(struct node (name weight parent children) #:transparent #:mutable)

(define (extract-values line)
  (let ((kids '()))
   (define parts (car (regexp-match* #rx"^([a-z]+) \\(([0-9]+)\\)( -> .*$)?" line #:match-select cdr)))

   (when (third parts)
     (define children (regexp-match* #rx",? ([a-z]+)+" (third parts) #:match-select cdr))
     (set! kids (flatten children)))

   (values (first parts) (second parts) kids)))

(define (fill-tree filename)
  (let ((lines (file->lines filename))
        (tree (make-hash)))

    (for ((line lines))
         (define-values (name weight kids) (extract-values line))
         (hash-set! tree name (node name (string->number weight) "" kids)))

    (for ((name (hash-keys tree)))
         (for ((kid (node-children (hash-ref tree name))))
              (hash-set! tree kid (struct-copy node (hash-ref tree kid) (parent name)))))

    tree))

(define (part-one filename)
  (let ((tree (fill-tree filename))
        (base ""))
   (for ((name (hash-keys tree)))
        (when (string=? (node-parent (hash-ref tree name)) "")
;          (printf "The base is ~a~n" name)
          (set! base name)))
   tree))

;(check-equal? (part-one "test-data.txt") "tknk")
;(part-one "day07-data.txt")

;
; --- Part Two ---
;
; The programs explain the situation: they can't get down. Rather, they
;  could get down, if they weren't expending all of their energy trying to
;  keep the tower balanced. Apparently, one program has the wrong weight,
;  and until it's fixed, they're stuck here.
;
; For any program holding a disc, each program standing on that disc forms
;  a sub-tower. Each of those sub-towers are supposed to be the same weight,
;  or the disc itself isn't balanced. The weight of a tower is the sum of the
;  weights of the programs in that tower.
;
; In the example above, this means that for ugml's disc to be balanced, gyxo,
;  ebii, and jptl must all have the same weight, and they do: 61.
;
; However, for tknk to be balanced, each of the programs standing on its disc
;  and all programs above it must each match. This means that the following
;  sums must all be the same:
;
; * ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
; * padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
; * fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243
;
; As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the
;  other two. Even though the nodes above ugml are balanced, ugml itself is too
;  heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep
;  the towers balanced. If this change were made, its weight would be 60.
;
; Given that exactly one program is the wrong weight, what would its weight
;  need to be to balance the entire tower? (Answer: 1079)
;

(define (get-base-name tree)
  (let ((base ""))
   (for ((name (hash-keys tree)))
        (when (string=? "" (node-parent (hash-ref tree name)))
          (set! base name)))
   base))

(define (get-weight tree base-name)
  (let ((base (hash-ref tree base-name)))
   (if (zero? (length (node-children base)))
       (node-weight base)
       (let ((weights (make-hash)))
         (for ((kid (node-children base)))
              (hash-set! weights kid (get-weight tree kid)))

         (let* ((numbers (hash-values weights))
                (unique (length (remove-duplicates numbers))))
           (printf "Unique numbers: ~a, name: ~a, weight: ~a, parent: ~a, weights: ~a, total: ~a~n"
                   unique base-name (node-weight base) (node-parent base) weights (+ (node-weight base) (apply + numbers)))
           (+ (node-weight base) (apply + numbers)))))))

(define (part-two filename)
  (let* ((tree (part-one filename))
         (base-name (get-base-name tree)))
    (get-weight tree base-name)))

;(part-two "test-data.txt")
(part-two "day07-data.txt")

(define (draw-tree filename)
  (printf "digraph G {~n")
  (printf "  ranksep=2.00;~n")
  (let ((tree (part-one filename))
        (drawn (make-hash)))
    (for ((name (hash-keys tree)))
         (when (not (hash-has-key? drawn name))
           (printf "  ~a [label=\"~a\\n(~a)\"];~n" name name (get-weight tree name))
           (hash-set! drawn name #t))
         (for ((kid (node-children (hash-ref tree name))))
              (when (not (hash-has-key? drawn name))
                (printf "  ~a [label=\"~a\\n(~a)\"];~n" kid kid (get-weight tree kid))
                (hash-set! drawn kid #t))
              (printf "  ~a -> ~a~n" name kid))))
  (printf "}~n"))

