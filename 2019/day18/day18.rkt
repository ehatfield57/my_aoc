#lang racket

(require racket/string
         racket/set
         data/heap
         racket/file
         racket/math)

;; Structure to represent the maze
(struct maze (grid width height keys doors start-pos) #:transparent)

;; Function to parse the maze from input string
(define (parse-maze input)
  (define lines (filter (λ (line) (non-empty-string? line)) (string-split input "\n")))
  (define height (length lines))
  (define width (if (empty? lines) 0 (string-length (first lines))))
  ;; Create a 2D grid as a vector of vectors
  (define grid
    (vector
     (for/list ([line lines])
       (string->list line))))
  (define keys (make-hash))   ;; Mapping from key character to its position [x, y]
  (define doors (make-hash))  ;; Mapping from door character to its position [x, y]
  (define start-pos #f)        ;; Starting position [x, y]
  ;; Populate keys, doors, and start position
  (for ([y (in-range height)]
        [x (in-range width)])
    (define char (list-ref (car (vector-ref grid y)) x))
    (printf "Hi Edward A: char:~a~n" char)
    (printf "Hi Edward A1: grid:~a~n" grid)
    (printf "Hi Edward A2 x:~a~n" x)
    (printf "Hi Edward A3 y:~a~n" y)
    (cond
      [(char=? char #\@)
       (set! start-pos (list x y))]
      [(and (char-lower-case? char) (char-alphabetic? char))
       (hash-set! keys char (list x y))]
      [(and (char-upper-case? char) (char-alphabetic? char))
       (hash-set! doors char (list x y))]))
  (maze grid width height keys doors start-pos))

;; Function to get neighbors (up, down, left, right) that are not walls
(define (neighbors x y maze)
  (define dirs '((0 . 1) (1 . 0) (0 . -1) (-1 . 0)))
  (define neighbor-positions
    (for/list ([dir dirs])
      (let ([nx (+ x (car dir))]
            [ny (+ y (cdr dir))])
        (printf "Hi Edward B: ~a~n" (list-ref (vector-ref (maze-grid maze) ny) nx))
        (printf "Hi Edward B1: maze:~a~n" maze)
        (printf "Hi Edward B2: maze-grid maze:~a~n" (maze-grid maze))
        (printf "Hi Edward B3: ny:~a, nx:~a~n" ny nx)
        (when (and (>= nx 0)
                   (< nx (maze-width maze))
                   (>= ny 0)
                   (< ny (maze-height maze))
                   (not (char=? (list-ref (vector-ref (maze-grid maze) ny) nx) #\#)))
          (list nx ny)))))
  (filter identity neighbor-positions))

;; Function to convert a list of [x, y] to a unique key string
(define (position-key pos)
  (format "~a,~a" (first pos) (second pos)))

;; Function to perform BFS from a position to find all reachable keys
(define (find-reachable-keys maze pos collected-keys)
  (define queue (list (cons pos 0)))  ;; Each element is (position . distance)
  (define visited (make-hash))        ;; Hash to track visited positions
  (hash-set! visited (position-key pos) #t)
  (define reachable '())              ;; List of (key . distance)
  (let loop ([queue queue] [visited visited])
    (if (null? queue)
      reachable
      (let* ([current (car queue)]
             [current-pos (car current)]
             [current-dist (cdr current)]
             [x (first current-pos)]
             [y (second current-pos)]
             [adj (neighbors x y maze)])
        (for ([n adj])
          (define nk (position-key n))
          (unless (hash-has-key? visited nk)
            (hash-set! visited nk #t)
            (define char (list-ref (vector-ref (maze-grid maze) (second n)) (first n)))
            (cond
              ;; If it's a key and not yet collected
              [(and (char-lower-case? char) (char-alphabetic? char)
                    (not (set-member? collected-keys char)))
               (set! reachable (cons (cons char (+ current-dist 1)) reachable))]
              ;; If it's a door, check if the corresponding key is collected
              [(and (char-upper-case? char) (char-alphabetic? char))
               (define required-key (char-downcase char))
               (when (set-member? collected-keys required-key)
                 (set! queue (append queue (list (cons n (+ current-dist 1))))))]
              ;; It's an open passage
              [else
               (set! queue (append queue (list (cons n (+ current-dist 1)))))])))
        (loop (cdr queue) visited)))))

;; Heuristic function: number of remaining keys
(define (heuristic remaining-key-count)
  remaining-key-count)

;; Function to count the number of set bits in an integer (for bitmask)
(define (bit-count n)
  (if (= n 0)
    0
    (+ (bit-count (bitwise-and n (- n))) 1)))

;; Function to check if all keys are collected
(define (all-keys-collected? collected-bitmask total-keys)
  (= collected-bitmask (arithmetic-shift 1 total-keys -1)))

;; A* Search Algorithm
(define (a-star maze)
  (define all-keys (hash-keys (maze-keys maze)))
  (define total-keys (length all-keys))
  ;; Assign each key a unique index for bitmask
  (define key-to-index
    (let loop ([keys (sort all-keys char<?)] [i 0] [h (make-hash)])
      (if (null? keys)
        h
        (loop (cdr keys) (+ i 1) (hash-set! h (car keys) i)))))
  ;; Initial state
  (define start-pos (maze-start-pos maze))
  (define initial-state (cons start-pos 0))  ;; (position . collected-bitmask)
  ;; Priority Queue: elements are (f, g, state)
  ;; f = g + h
  ;; g = cost so far
  ;; state = (position . collected-bitmask)
  (define pq (make-heap
              (λ (a b)
                (< (first a) (first b)))))  ;; Min-heap based on f
  ;; Initial heuristic
  (define initial-h (heuristic total-keys))
  (heap-add! pq (list (+ 0 initial-h) 0 initial-state))
  ;; Visited states: hash mapping from (position . collected-bitmask) to g
  (define visited (make-hash))
  (hash-set! visited initial-state 0)
  ;; Main loop
  (let loop ()
    (if (zero? (heap-count pq))
      #f  ;; No solution found
      (let* ([current (heap-remove-min! pq)]
             [f (first current)]
             [g (second current)]
             [state (third current)]
             [current-pos (car state)]
             [current-bitmask (cdr state)])
        ;; Check if all keys are collected
        (if (all-keys-collected? current-bitmask total-keys)
          g  ;; Return the number of steps
          (let* ([reachable-keys (find-reachable-keys maze current-pos (set (map char-downcase all-keys)))])
            (for ([rk reachable-keys])
              (define key (car rk))
              (define dist (cdr rk))
              ;; Update collected keys bitmask
              (define key-index (hash-ref key-to-index key))
              (define new-bitmask (bitwise-ior current-bitmask (arithmetic-shift 1 key-index)))
              ;; Find the position of the new key
              (define key-pos (hash-ref (maze-keys maze) key))
              ;; Define new state
              (define new-state (cons key-pos new-bitmask))
              ;; Check if this state has been visited with a lower g
              (define existing-g (hash-ref visited new-state #f))
              (when (or (not existing-g) (< (+ g dist) existing-g))
                (hash-set! visited new-state (+ g dist))
                ;; Calculate heuristic: remaining keys count
                (define remaining-keys (- total-keys (bit-count new-bitmask)))
                (define new-h (heuristic remaining-keys))
                ;; Add to priority queue
                (heap-add! pq (list (+ (+ g dist) new-h) (+ g dist) new-state))))
            ;; Continue the loop
            (loop)))))))

;; Main function to read input and execute A* search
(define (main filename)
  (define input (file->string filename))
  (define my-maze (parse-maze input))
  ;; Run A* to find the minimum number of steps to collect all keys
  (define result (a-star my-maze))
  (if result
    (printf "Minimum number of steps to collect all keys: ~a\n" result)
    (printf "No solution found.\n")))

;; Execute the main function
(main "test-data1.txt")
