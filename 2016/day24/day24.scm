
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 24: Air Duct Spelunking ---
;
; You've finally met your match; the doors that provide access to the roof are locked
;  tight, and all of the controls and related electronics are inaccessible. You simply
;  can't reach them.
;
; The robot that cleans the air ducts, however, can.
;
; It's not a very fast little robot, but you reconfigure it to be able to interface with
;  some of the exposed wires that have been routed through the HVAC system. If you can
;  direct it to each of those locations, you should be able to bypass the security controls.
;
; You extract the duct layout for this area from some blueprints you acquired and create
;  a map with the relevant locations marked (your puzzle input). 0 is your current location,
;  from which the cleaning robot embarks; the other numbers are (in no particular order)
;  the locations the robot needs to visit at least once each. Walls are marked as #, and
;  open passages are marked as .. Numbers behave like open passages.
;
; For example, suppose you have a map like the following:
;
; ###########
; #0.1.....2#
; #.#######.#
; #4.......3#
; ###########
;
; To reach all of the points of interest as quickly as possible, you would have the robot take
;  the following path:
;
; 0 to 4 (2 steps)
; 4 to 1 (4 steps; it can't move diagonally)
; 1 to 2 (6 steps)
; 2 to 3 (2 steps)
;
; Since the robot isn't very fast, you need to find it the shortest route. This path is the fewest
;  steps (in the above example, a total of 14) required to start at 0 and then visit every other
;  location at least once.
;
; Given your actual map, and starting from location 0, what is the fewest number of steps required
;  to visit every non-0 number marked on the map at least once?  (Answer: 430)
;

(define max-x 0)
(define max-y 0)
(define max-loc 0)
(define digit-loc (make-hash))

(define (read-maze filename)
  (let ((maze-hash (make-hash))
        (x 0) (y 0))
    (with-input-from-file filename
                          (lambda ()
                            (let loop ((c (read-char)))
                             (when (not (eof-object? c))
                               (cond
                                 ((char=? c #\newline)
                                  (set! max-x x)
                                  (set! x 0)
                                  (set! y (add1 y))
                                  (set! max-y y))
                                 (else
                                   (hash-set! maze-hash (cons x y) c)
                                   (when (regexp-match #rx"[0-9]" (string c))
                                     (hash-set! digit-loc (string->number (string c)) (cons x y))
                                     (hash-set! digit-loc (cons x y) (string->number (string c)))
                                     (let ((c-num (string->number (string c))))
                                       (when (> c-num max-loc) (set! max-loc c-num))))
                                   (set! x (add1 x))))
                               (loop (read-char))))))
    maze-hash))

(define (make-tree maze-hash)
  (let ((maze-tree (make-hash)))
    (for ((y (range max-y)))
         (for ((x (range max-x)))
              (let ((c (hash-ref maze-hash (cons x y)))
                    (neighbors '()))
                (when (not (char=? c #\#))
                  (let ((east-key  (cons (add1 x) y))
                        (south-key (cons x (add1 y)))
                        (west-key  (cons (sub1 x) y))
                        (north-key (cons x (sub1 y))))
                    (when (not (char=? #\# (hash-ref maze-hash east-key)))  (set! neighbors (cons east-key neighbors)))
                    (when (not (char=? #\# (hash-ref maze-hash south-key))) (set! neighbors (cons south-key neighbors)))
                    (when (not (char=? #\# (hash-ref maze-hash west-key)))  (set! neighbors (cons west-key neighbors)))
                    (when (not (char=? #\# (hash-ref maze-hash north-key))) (set! neighbors (cons north-key neighbors)))))

                (when (not (null? neighbors))
                  (hash-set! maze-tree (cons x y) neighbors)))))
    maze-tree))

;
; Convert maze-tree to dist-hash: (key: 0) '((1 . 10) (2 . 3) ...)
;

(define dist-hash (make-hash))

(define (dist-depth maze-tree visited neighbors idx depth)
  (let ((new-neighbors '()))
   (for ((neighbor neighbors))
        (when (not (hash-has-key? visited neighbor))
          (when (hash-has-key? digit-loc neighbor)
            (if (hash-has-key? dist-hash idx)
                (hash-set! dist-hash idx (cons (cons (hash-ref digit-loc neighbor) depth) (hash-ref dist-hash idx)))
                (hash-set! dist-hash idx (list (cons (hash-ref digit-loc neighbor) depth)))))
          (set! new-neighbors (append (hash-ref maze-tree neighbor) new-neighbors))
          (hash-set! visited neighbor depth)))
   (when (not (zero? (length new-neighbors))) (dist-depth maze-tree visited new-neighbors idx (add1 depth)))
   ))


(define (calc-dist maze-tree)
  (for ((idx (range (add1 max-loc))))
       (define visited (make-hash))
       (dist-depth maze-tree visited (list (hash-ref digit-loc idx)) idx 0)))

(define (part-one filename)
  (let* ((maze-hash (read-maze filename))
         (maze-tree (make-tree maze-hash)))
    (calc-dist maze-tree)
    dist-hash))

;(part-one "day24-data.txt")

;
; --- Part Two ---
;
; Of course, if you leave the cleaning robot somewhere weird, someone is bound
;  to notice.
;
; What is the fewest number of steps required to start at 0, visit every non-0
;  number marked on the map at least once, and then return to 0? (Answer: 700)
;

; 712 is too big. :-/

(define (tally dist-hash perm)
  (let ((total 0)
        (curr  (car perm))
        (other (cdr perm)))
   (for ((next other))
        (define dist (cdr (assoc next (hash-ref dist-hash curr))))
        (set! total (+ total dist))
        (set! curr next))
   total))

(define (part-two filename)
  (let* ((dist-hash (part-one filename))
         (perms (filter (lambda (n) (zero? (car n))) (permutations (range 8))))
         (min-sum 712))
    (for ((lst perms))
         (define total (tally dist-hash (reverse (cons 0 (reverse lst)))))
         (printf "~a <- ~a~n" total lst)
         (when (< total min-sum) (set! min-sum total)))
    min-sum))

(part-two "day24-data.txt") ; -> 700

