
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 13: Mine Cart Madness ---
;
; A crop of this size requires significant logistics to transport produce,
;  soil, fertilizer, and so on. The Elves are very busy pushing things around
;  in carts on some kind of rudimentary system of tracks they've come up with.
;
; Seeing as how cart-and-track systems don't appear in recorded history for
;  another 1000 years, the Elves seem to be making this up as they go along.
;  They haven't even figured out how to avoid collisions yet.
;
; You map out the tracks (your puzzle input) and see where you can help.
;
; Tracks consist of straight paths (| and -), curves (/ and \), and intersections
;  (+). Curves connect exactly two perpendicular pieces of track; for example,
;  this is a closed loop:
;
; /----\
; |    |
; |    |
; \----/
;
; Intersections occur when two perpendicular paths cross. At an intersection, a
;  cart is capable of turning left, turning right, or continuing straight. Here
;  are two loops connected by two intersections:
;
; /-----\
; |     |
; |  /--+--\
; |  |  |  |
; \--+--/  |
;    |     |
;    \-----/
;
; Several carts are also on the tracks. Carts always face either up (^), down (v),
;  left (<), or right (>). (On your initial map, the track under each cart is a
;  straight path matching the direction the cart is facing.)
;
; Each time a cart has the option to turn (by arriving at any intersection), it turns
;  left the first time, goes straight the second time, turns right the third time,
;  and then repeats those directions starting again with left the fourth time, straight
;  the fifth time, and so on. This process is independent of the particular intersection
;  at which the cart has arrived - that is, the cart has no per-intersection memory.
;
; Carts all move at the same speed; they take turns moving a single step at a time. They
;  do this based on their current location: carts on the top row move first (acting from
;  left to right), then carts on the second row move (again from left to right), then
;  carts on the third row, and so on. Once each cart has moved one step, the process
;  repeats; each of these loops is called a tick.
;
; For example, suppose there are two carts on a straight track:
;
; |  |  |  |  |
; v  |  |  |  |
; |  v  v  |  |
; |  |  |  v  X
; |  |  ^  ^  |
; ^  ^  |  |  |
; |  |  |  |  |
;
; First, the top cart moves. It is facing down (v), so it moves down one square. Second,
;  the bottom cart moves. It is facing up (^), so it moves up one square. Because all
;  carts have moved, the first tick ends. Then, the process repeats, starting with the
;  first cart. The first cart moves down, then the second cart moves up - right into
;  the first cart, colliding with it! (The location of the crash is marked with an X.)
;  This ends the second and last tick.
;
; Here is a longer example:
;
; /->-\        
; |   |  /----\
; | /-+--+-\  |
; | | |  | v  |
; \-+-/  \-+--/
;   \------/   
;
; /-->\        
; |   |  /----\
; | /-+--+-\  |
; | | |  | |  |
; \-+-/  \->--/
;   \------/   
;
; /---v        
; |   |  /----\
; | /-+--+-\  |
; | | |  | |  |
; \-+-/  \-+>-/
;   \------/   
;
; /---\        
; |   v  /----\
; | /-+--+-\  |
; | | |  | |  |
; \-+-/  \-+->/
;   \------/   
;
; /---\        
; |   |  /----\
; | /->--+-\  |
; | | |  | |  |
; \-+-/  \-+--^
;   \------/   
;
; /---\        
; |   |  /----\
; | /-+>-+-\  |
; | | |  | |  ^
; \-+-/  \-+--/
;   \------/   
;
; /---\        
; |   |  /----\
; | /-+->+-\  ^
; | | |  | |  |
; \-+-/  \-+--/
;   \------/   
;
; /---\        
; |   |  /----<
; | /-+-->-\  |
; | | |  | |  |
; \-+-/  \-+--/
;   \------/   
;
; /---\        
; |   |  /---<\
; | /-+--+>\  |
; | | |  | |  |
; \-+-/  \-+--/
;   \------/   
;
; /---\        
; |   |  /--<-\
; | /-+--+-v  |
; | | |  | |  |
; \-+-/  \-+--/
;   \------/   
;
; /---\        
; |   |  /-<--\
; | /-+--+-\  |
; | | |  | v  |
; \-+-/  \-+--/
;   \------/   
;
; /---\        
; |   |  /<---\
; | /-+--+-\  |
; | | |  | |  |
; \-+-/  \-<--/
;   \------/   
;
; /---\        
; |   |  v----\
; | /-+--+-\  |
; | | |  | |  |
; \-+-/  \<+--/
;   \------/   
;
; /---\        
; |   |  /----\
; | /-+--v-\  |
; | | |  | |  |
; \-+-/  ^-+--/
;   \------/   
;
; /---\        
; |   |  /----\
; | /-+--+-\  |
; | | |  X |  |
; \-+-/  \-+--/
;   \------/   
;
; After following their respective paths for a while, the carts eventually crash.
;  To help prevent crashes, you'd like to know the location of the first crash.
;  Locations are given in X,Y coordinates, where the furthest left column is X=0
;  and the furthest top row is Y=0:
;
;            111
;  0123456789012
; 0/---\        
; 1|   |  /----\
; 2| /-+--+-\  |
; 3| | |  X |  |
; 4\-+-/  \-+--/
; 5  \------/   
;
; In this example, the location of the first crash is 7,3. (Answer: 86,118)
;

(struct cart (coors dir dir-cnt) #:mutable #:transparent)

(define (load-data filename)
  (let ((track (make-hash))
        (input (open-input-file filename))
        (carts (make-hash))
        (x 0) (y 0) (cart-idx 0))
    (let loop ((ch (read-char input)))
         (case ch
           ((#\>)
            (hash-set! track (cons x y) #\-)
            (hash-set! carts cart-idx (cart (cons x y) 'east 0))
            (set! cart-idx (add1 cart-idx)))
           ((#\<)
            (hash-set! track (cons x y) #\-)
            (hash-set! carts cart-idx (cart (cons x y) 'west 0))
            (set! cart-idx (add1 cart-idx)))
           ((#\v)
            (hash-set! track (cons x y) #\|)
            (hash-set! carts cart-idx (cart (cons x y) 'south 0))
            (set! cart-idx (add1 cart-idx)))
           ((#\^)
            (hash-set! track (cons x y) #\|)
            (hash-set! carts cart-idx (cart (cons x y) 'north 0))
            (set! cart-idx (add1 cart-idx)))
           ((#\newline)
            (set! y (add1 y))
            (set! x -1))
           (else
             (when (not (char=? ch #\space))
               (hash-set! track (cons x y) ch))))
         (set! x (add1 x))
         (set! ch (read-char input))
         (when (not (eof-object? ch))
           (loop ch)))
    (close-input-port input)
    (values track carts)))

(define (turn dir-cnt) (vector-ref (vector 'left 'straight 'right) (modulo dir-cnt 3)))
(define (go-north y) (values 'north (sub1 y)))
(define (go-south y) (values 'south (add1 y)))
(define (go-east  x) (values 'east  (add1 x)))
(define (go-west  x) (values 'west  (sub1 x)))

(define (compare-pairs p1 p2) ; Thanks Gemini!
  (cond
    [(< (cdr p1) (cdr p2)) #t]
    [(> (cdr p1) (cdr p2)) #f]
    [else (< (car p1) (car p2))]))

(define (tick track carts)
  (define sorted-cart-keys
    (sort
      (hash-keys carts)
      (lambda (a b) (compare-pairs (cart-coors (hash-ref carts a)) (cart-coors (hash-ref carts b))))))
 
  (for ((c sorted-cart-keys))
       (let* ((cart-info (hash-ref carts c))
              (sym (hash-ref track (cart-coors cart-info)))
              (dir (cart-dir cart-info))
              (coor (cart-coors cart-info))
              (x (car coor))
              (y (cdr coor))
              (dir-cnt (cart-dir-cnt cart-info)))
         (case sym
           ((#\-)
            (if (eq? dir 'east)
                (set! x (add1 x))
                (set! x (sub1 x))))
           ((#\|)
            (if (eq? dir 'south)
                (set! y (add1 y))
                (set! y (sub1 y))))
           ((#\\)
            (case dir
              ((north) (set!-values (dir x) (go-west  x)))
              ((south) (set!-values (dir x) (go-east  x)))
              ((east)  (set!-values (dir y) (go-south y)))
              ((west)  (set!-values (dir y) (go-north y)))))
           ((#\/)
            (case dir
              ((north) (set!-values (dir x) (go-east  x)))
              ((south) (set!-values (dir x) (go-west  x)))
              ((east)  (set!-values (dir y) (go-north y)))
              ((west)  (set!-values (dir y) (go-south y)))))
           ((#\+)
            (case (turn dir-cnt)
              ((left)
               (case dir
                 ((north) (set!-values (dir x) (go-west  x)))
                 ((south) (set!-values (dir x) (go-east  x)))
                 ((east)  (set!-values (dir y) (go-north y)))
                 ((west)  (set!-values (dir y) (go-south y)))))
              ((straight)
               (case dir
                 ((north) (set!-values (dir y) (go-north y)))
                 ((south) (set!-values (dir y) (go-south y)))
                 ((east)  (set!-values (dir x) (go-east  x)))
                 ((west)  (set!-values (dir x) (go-west  x)))))
              ((right)
               (case dir
                 ((north) (set!-values (dir x) (go-east  x)))
                 ((south) (set!-values (dir x) (go-west  x)))
                 ((east)  (set!-values (dir y) (go-south y)))
                 ((west)  (set!-values (dir y) (go-north y)))))
              )
            (set! dir-cnt (add1 dir-cnt))))
         (hash-set! carts c (cart (cons x y) dir dir-cnt)))))

(define (collision carts)
  (let ((bang #f))
   (for ((pair (combinations (map (lambda (c) (cart-coors (hash-ref carts c))) (hash-keys carts)) 2)))
        (when (equal? (car pair) (cadr pair))
          (set! bang (car pair))))
   bang))

(define (get-cart-by-coor carts coor)
  (let ((found #f))
   (for ((cart-key (hash-keys carts)))
        (when (equal? coor (cart-coors (hash-ref carts cart-key)))
          (set! found (hash-ref carts cart-key))))
   found))

(define (collisions carts)
  (let ((bang '())
        (carts-xys (map (lambda (c) (cart-coors (hash-ref carts c))) (hash-keys carts))))
    (for ((pair (combinations carts-xys 2)))
         (when (equal? (car pair) (cadr pair))
           (set! bang (cons (car pair) bang))))
    ; TBD - Subtle bug when two carts phase through each other --><--, --<>-- 
    (for ((pair (combinations carts-xys 2)))
         (define-values (x1 y1 x2 y2) (apply values (flatten pair)))
         (when (or (and (= 0 (- x2 x1)) (= 1 (- y2 y1)))
                   (and (= 1 (- x2 x1)) (= 0 (- y2 y1))))
           (when (not (equal? (car pair) (cadr pair)))
             (let ((dir1 (cart-dir (get-cart-by-coor carts (car pair))))
                   (dir2 (cart-dir (get-cart-by-coor carts (cadr pair)))))
               (when (or (and (eq? dir1 'north) (eq? dir2 'south))
                         (and (eq? dir1 'south) (eq? dir2 'north))
                         (and (eq? dir1 'west)  (eq? dir2 'east))
                         (and (eq? dir1 'east)  (eq? dir2 'west)))
                 (set! bang (cons (car pair) bang))
                 (set! bang (cons (cadr pair) bang))
                 (printf "Hi Edward B, pair:~a diff-x:~a (~a), diff-y:~a (~a)~n"
                         pair
                         (- x2 x1)
                         (cart-dir (get-cart-by-coor carts (car pair)))
                         (- y2 y1)
                         (cart-dir (get-cart-by-coor carts (cadr pair)))
                         ))))))
    bang))

(define (dir-symbol dir)
  (case dir
    ((north) "^")
    ((south) "v")
    ((east)  ">")
    ((west)  "<")))

(define (draw-track track carts)
  (let ((max-x (apply max (map car (hash-keys track))))
        (max-y (apply max (map cdr (hash-keys track))))
        (cart-lookup (make-hash)))
    (for ((k (hash-keys carts)))
         (define cart-info (hash-ref carts k))
         (hash-set! cart-lookup (cart-coors cart-info) (dir-symbol (cart-dir cart-info))))

    (define crashes (collisions carts))
    (for ((y (range (add1 max-y))))
         (for ((x (range (add1 max-x))))
              (if (not (hash-has-key? track (cons x y)))
                  (printf " ")
                  (if (hash-has-key? cart-lookup (cons x y))
                      (if (member (cons x y) crashes)
                          (printf "X")
                          (printf "~a" (hash-ref cart-lookup (cons x y))))
                      (printf "~a" (hash-ref track (cons x y))))))
         (printf "~n"))
    (printf "~n")))

(define (part-one filename)
  (define-values (track carts) (load-data filename))
  (draw-track track carts)
  (let loop ((t 0))
   (tick track carts)
   (draw-track track carts)
   (if (collision carts)
       (printf "Collision at tick ~a at coor: ~a~n" t (collision carts))
       (loop (add1 t)))))

;
; --- Part Two ---
;
; There isn't much you can do to prevent crashes in this ridiculous system.
;  However, by predicting the crashes, the Elves know where to be in advance
;  and instantly remove the two crashing carts the moment any crash occurs.
;
; They can proceed like this for a while, but eventually, they're going to
;  run out of carts. It could be useful to figure out where the last cart
;  that hasn't crashed will end up.
;
; For example:
;
; />-<\  
; |   |  
; | /<+-\
; | | | v
; \>+</ |
;   |   ^
;   \<->/
;
; /---\  
; |   |  
; | v-+-\
; | | | |
; \-+-/ |
;   |   |
;   ^---^
;
; /---\  
; |   |  
; | /-+-\
; | v | |
; \-+-/ |
;   ^   ^
;   \---/
;
; /---\  
; |   |  
; | /-+-\
; | | | |
; \-+-/ ^
;   |   |
;   \---/
;
; After four very expensive crashes, a tick ends with only one cart remaining; its
;  final location is 6,4.
;
; What is the location of the last cart at the end of the first tick where it is
;  the only cart left? (Answer: 2,81)
;

(define (part-two filename)
  (define-values (track carts) (load-data filename))
;  (draw-track track carts)
  (let loop ((t 0))
   (tick track carts)
;   (draw-track track carts)
   (define crashes (collisions carts))
   (when (> (length crashes) 0)
     (for ((crash crashes))
          (printf "Collision at tick ~a at coor: ~a~n" t crash)
          (for ((c (hash-keys carts)))
               (when (equal? crash (cart-coors (hash-ref carts c)))
                 (hash-remove! carts c)))))
   (if (= 1 (length (hash-keys carts)))
       (printf "Last cart standing: ~a~n" (hash-ref carts (car (hash-keys carts))))
       (loop (add1 t)))))

; Not (147 . 59)
; Not (36 . 144)

