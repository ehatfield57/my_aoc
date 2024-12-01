
(require rackunit)
(require graph)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 20: A Regular Map ---
;
; While you were learning about instruction pointers, the Elves made considerable
;  progress. When you look up, you discover that the North Pole base construction
;  project has completely surrounded you.
;
; The area you are in is made up entirely of rooms and doors. The rooms are arranged
;  in a grid, and rooms only connect to adjacent rooms when a door is present between
;  them.
;
; For example, drawing rooms as ., walls as #, doors as | or -, your current position
;  as X, and where north is up, the area you're in might look like this:
;
; #####
; #.|.#
; #-###
; #.|X#
; #####
;
; You get the attention of a passing construction Elf and ask for a map. "I don't have
;  time to draw out a map of this place - it's huge. Instead, I can give you directions
;  to every room in the facility!" He writes down some directions on a piece of parchment
;  and runs off. In the example above, the instructions might have been ^WNE$, a regular
;  expression or "regex" (your puzzle input).
;
; The regex matches routes (like WNE for "west, north, east") that will take you from
;  your current room through various doors in the facility. In aggregate, the routes
;  will take you through every door in the facility at least once; mapping out all of
;  these routes will let you build a proper map and find your way around.
;
; ^ and $ are at the beginning and end of your regex; these just mean that the regex
;  doesn't match anything outside the routes it describes. (Specifically, ^ matches
;  the start of the route, and $ matches the end of it.) These characters will not
;  appear elsewhere in the regex.
;
; The rest of the regex matches various sequences of the characters N (north), S (south),
;  E (east), and W (west). In the example above, ^WNE$ matches only one route, WNE, which
;  means you can move west, then north, then east from your current position. Sequences
;  of letters like this always match that exact route in the same order.
;
; Sometimes, the route can branch. A branch is given by a list of options separated by
;  pipes (|) and wrapped in parentheses. So, ^N(E|W)N$ contains a branch: after going
;  north, you must choose to go either east or west before finishing your route by going
;  north again. By tracing out the possible routes after branching, you can determine
;  where the doors are and, therefore, where the rooms are in the facility.
;
; For example, consider this regex: ^ENWWW(NEEE|SSE(EE|N))$
;
; This regex begins with ENWWW, which means that from your current position, all routes
;  must begin by moving east, north, and then west three times, in that order. After this,
;  there is a branch. Before you consider the branch, this is what you know about the map
;  so far, with doors you aren't sure about marked with a ?:
;
; #?#?#?#?#
; ?.|.|.|.?
; #?#?#?#-#
;     ?X|.?
;     #?#?#
;
; After this point, there is (NEEE|SSE(EE|N)). This gives you exactly two options: NEEE and
;  SSE(EE|N). By following NEEE, the map now looks like this:
;
; #?#?#?#?#
; ?.|.|.|.?
; #-#?#?#?#
; ?.|.|.|.?
; #?#?#?#-#
;     ?X|.?
;     #?#?#
;
; Now, only SSE(EE|N) remains. Because it is in the same parenthesized group as NEEE, it starts
;  from the same room NEEE started in. It states that starting from that point, there exist doors
;  which will allow you to move south twice, then east; this ends up at another branch. After
;  that, you can either move east twice or north once. This information fills in the rest of the
;  doors:
;
; #?#?#?#?#
; ?.|.|.|.?
; #-#?#?#?#
; ?.|.|.|.?
; #-#?#?#-#
; ?.?.?X|.?
; #-#-#?#?#
; ?.|.|.|.?
; #?#?#?#?#
;
; Once you've followed all possible routes, you know the remaining unknown parts are all walls,
;  producing a finished map of the facility:
;
; #########
; #.|.|.|.#
; #-#######
; #.|.|.|.#
; #-#####-#
; #.#.#X|.#
; #-#-#####
; #.|.|.|.#
; #########
;
; Sometimes, a list of options can have an empty option, like (NEWS|WNSE|). This means that routes
;  at this point could effectively skip the options in parentheses and move on immediately. For
;  example, consider this regex and the corresponding map:
;
; ^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$
;
; ###########
; #.|.#.|.#.#
; #-###-#-#-#
; #.|.|.#.#.#
; #-#####-#-#
; #.#.#X|.#.#
; #-#-#####-#
; #.#.|.|.|.#
; #-###-###-#
; #.|.|.#.|.#
; ###########
;
; This regex has one main route which, at three locations, can optionally include additional detours
;  and be valid: (NEWS|), (WNSE|), and (SWEN|). Regardless of which option is taken, the route
;  continues from the position it is left at after taking those steps. So, for example, this regex
;  matches all of the following routes (and more that aren't listed here):
;
; ENNWSWWSSSEENEENNN
; ENNWSWWNEWSSSSEENEENNN
; ENNWSWWNEWSSSSEENEESWENNNN
; ENNWSWWSSSEENWNSEEENNN
;
; By following the various routes the regex matches, a full map of all of the doors and rooms in the
;  facility can be assembled.
;
; To get a sense for the size of this facility, you'd like to determine which room is furthest from
;  you: specifically, you would like to find the room for which the shortest path to that room would
;  require passing through the most doors.
;
; In the first example (^WNE$), this would be the north-east corner 3 doors away.
; In the second example (^ENWWW(NEEE|SSE(EE|N))$), this would be the south-east corner 10 doors away.
; In the third example (^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$), this would be the north-east corner 18 doors away.
;
; Here are a few more examples:
;
; Regex: ^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$
; Furthest room requires passing 23 doors
;
; #############
; #.|.|.|.|.|.#
; #-#####-###-#
; #.#.|.#.#.#.#
; #-#-###-#-#-#
; #.#.#.|.#.|.#
; #-#-#-#####-#
; #.#.#.#X|.#.#
; #-#-#-###-#-#
; #.|.#.|.#.#.#
; ###-#-###-#-#
; #.|.#.|.|.#.#
; #############
;
; Regex: ^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$
; Furthest room requires passing 31 doors
;
; ###############
; #.|.|.|.#.|.|.#
; #-###-###-#-#-#
; #.|.#.|.|.#.#.#
; #-#########-#-#
; #.#.|.|.|.|.#.#
; #-#-#########-#
; #.#.#.|X#.|.#.#
; ###-#-###-#-#-#
; #.|.#.#.|.#.|.#
; #-###-#####-###
; #.|.#.|.|.#.#.#
; #-#-#####-#-#-#
; #.#.|.|.|.#.|.#
; ###############
;
; What is the largest number of doors you would be required to pass through to reach a room?
;  That is, find the room for which the shortest path from your starting location to that room
;  would require passing through the most doors; what is the fewest doors you can pass through
;  to reach it? (Answer: 3739)
;

(define (make-coor x y dir)
  (case dir
    ((#\N) (cons       x (sub1 y)))
    ((#\E) (cons (add1 x)      y))
    ((#\S) (cons       x (add1 y)))
    ((#\W) (cons (sub1 x)      y))))

(define (process-input str-port the-map orig-coor)
  (let ((x (car orig-coor))
        (y (cdr orig-coor))
        (new-coor? (lambda (coor)
                     (when (not (hash-has-key? the-map coor))
                       (hash-set! the-map coor (make-hash '((#\N . #f) (#\E . #f) (#\S . #f) (#\W . #f) (#\c . ".")))))))
        (opposite-dir (lambda (dir) (case dir ((#\N) #\S) ((#\S) #\N) ((#\E) #\W) ((#\W) #\E)))))
    (let loop ((ch (read-char str-port)))
     (when (not (eof-object? ch))
       (new-coor? (cons x y))
       (case ch
         ((#\^ #\$) (loop (read-char str-port)))

         ((#\N #\E #\S #\W)
          (let ((new-coor (make-coor x y ch)))
           (new-coor? new-coor)
           (when (not (hash-ref (hash-ref the-map (cons x y)) ch))
             (hash-set! (hash-ref the-map (cons x y)) ch new-coor)
             (hash-set! (hash-ref the-map new-coor) (opposite-dir ch) (cons x y)))
           (set! x (car new-coor))
           (set! y (cdr new-coor)))
          (loop (read-char str-port)))

         ((#\()
          (process-input str-port the-map (cons x y))
          (loop (read-char str-port)))
         ((#\)) (void))
         ((#\|)
          (set! x (car orig-coor))
          (set! y (cdr orig-coor))
          (loop (read-char str-port)))

         (else (error (format "Unknown character: '~a'" c))))))))

(define (draw-map the-map)
  (let* ((coors (hash-keys the-map))
         (min-x (apply min (map (lambda (coor) (car coor)) coors)))
         (max-x (apply max (map (lambda (coor) (car coor)) coors)))
         (min-y (apply min (map (lambda (coor) (cdr coor)) coors)))
         (max-y (apply max (map (lambda (coor) (cdr coor)) coors)))
         (east #f))
    (hash-set! (hash-ref the-map (cons 0 0)) #\c "X") ; Set origin visually
    (for ((y (range min-y (add1 max-y))))
         (for ((x (range min-x (add1 max-x))))
              (if (hash-has-key? the-map (cons x y))
                  (let ((dirs (hash-ref the-map (cons x y))))
                   (printf "#~a"   (if (hash-ref dirs #\N) #\- #\#)))
                  (printf "##")))
         (printf "#~n")

         (for ((x (range min-x (add1 max-x))))
              (if (hash-has-key? the-map (cons x y))
                  (let ((dirs (hash-ref the-map (cons x y))))
                   (set! east (hash-ref dirs #\E))
                   (printf "~a~a" (if (hash-ref dirs #\W) #\| #\#)
                           (if (hash-ref dirs #\c) (hash-ref dirs #\c) #\#)))
                  (printf "##")))
         (printf "#~n")

         (when (= y max-y)
           (for ((x (range min-x (add1 max-x))))
                (if (hash-has-key? the-map (cons x y))
                    (let ((dirs (hash-ref the-map (cons x y))))
                     (printf "#~a"   (if (hash-ref dirs #\S) #\- #\#)))
                    (printf "##")))))
    (printf "#~n")))

(define (convert-to-graph the-map)
  (let ((vertices '())
        (lookup (make-hash)))
    (for ((coor (hash-keys the-map)))
         (for ((dir (list #\N #\E #\S #\W)))
              (let ((to-coor (hash-ref (hash-ref the-map coor) dir)))
               (when (and (hash-ref (hash-ref the-map coor) dir)
                          (not (hash-has-key? lookup (cons coor to-coor))))
                 (hash-set! lookup (cons coor to-coor) #t)
                 (hash-set! lookup (cons to-coor coor) #t)
                 (set! vertices (cons (list coor to-coor) vertices))
                 (set! vertices (cons (list to-coor coor) vertices))))))
    (unweighted-graph/undirected vertices)))

(define (process-string the-map in-str)
  (process-input (open-input-string in-str) the-map (cons 0 0)))

(define (part-one filename)
  (let ((the-map (make-hash))
        (in-str (file->string filename)))
;    (printf "Regex: '~a'~n" in-str)
    (process-string the-map in-str)
;    (draw-map the-map)
    (define-values (distances linkages) (bfs (convert-to-graph the-map) (cons 0 0)))
    (apply max (hash-values distances))))

(check-equal? (part-one "test2-data.txt") 18)
(check-equal? (part-one "test3-data.txt") 23)
(check-equal? (part-one "test4-data.txt") 31)
(check-equal? (part-one "day20-data.txt") 3739)

;
; Okay, so the facility is big.
;
; How many rooms have a shortest path from your current location
;  that pass through at least 1000 doors? (Answer: 8409)
;

(define (part-two)
  (let ((the-map (make-hash))
        (in-str (file->string "day20-data.txt")))
    (process-string the-map in-str)
    (define-values (distances linkages) (bfs (convert-to-graph the-map) (cons 0 0)))
    (filter (lambda (key)
              (let ((val (hash-ref distances key))) (and val (>= val 1000))))
            (hash-keys distances))))

(check-equal? (length (part-two)) 8409)

