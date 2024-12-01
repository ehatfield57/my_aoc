
(require rackunit)
(require racket/generator)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 13: Packet Scanners ---
;
; You need to cross a vast firewall. The firewall consists of several layers,
;  each with a security scanner that moves back and forth across the layer.
;  To succeed, you must not be detected by a scanner.
;
; By studying the firewall briefly, you are able to record (in your puzzle input)
;  the depth of each layer and the range of the scanning area for the scanner within
;  it, written as depth: range. Each layer has a thickness of exactly 1. A layer at
;  depth 0 begins immediately inside the firewall; a layer at depth 1 would start
;  immediately after that.
;
; For example, suppose you've recorded the following:
;
; 0: 3
; 1: 2
; 4: 4
; 6: 4
;
; This means that there is a layer immediately inside the firewall (with range 3),
;  a second layer immediately after that (with range 2), a third layer which begins
;  at depth 4 (with range 4), and a fourth layer which begins at depth 6 (also with
;  range 4). Visually, it might look like this:
;
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
; Within each layer, a security scanner moves back and forth within its range. Each
;  security scanner starts at the top and moves down until it reaches the bottom,
;  then moves up until it reaches the top, and repeats. A security scanner takes one
;  picosecond to move one step. Drawing scanners as S, the first few picoseconds
;  look like this:
;
;
; Picosecond 0:
;  0   1   2   3   4   5   6
; [S] [S] ... ... [S] ... [S]
; [ ] [ ]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
; Picosecond 1:
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... [ ] ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
; Picosecond 2:
;  0   1   2   3   4   5   6
; [ ] [S] ... ... [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [S]             [S]     [S]
;                 [ ]     [ ]
;
; Picosecond 3:
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... [ ] ... [ ]
; [S] [S]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [S]     [S]
;
; Your plan is to hitch a ride on a packet about to move through the firewall.
;  The packet will travel along the top of each layer, and it moves at one layer
;  per picosecond. Each picosecond, the packet moves one layer forward (its first
;  move takes it into layer 0), and then the scanners move one step. If there is
;  a scanner at the top of the layer as your packet enters it, you are caught.
;  (If a scanner moves into the top of its layer while you are there, you are not
;  caught: it doesn't have time to notice you before you leave.) If you were to
;  do this in the configuration above, marking your current position with parentheses,
;  your passage through the firewall would look like this:
;
; Initial state:
;  0   1   2   3   4   5   6
; [S] [S] ... ... [S] ... [S]
; [ ] [ ]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
; Picosecond 0:
;  0   1   2   3   4   5   6
; (S) [S] ... ... [S] ... [S]
; [ ] [ ]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; ( ) [ ] ... ... [ ] ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;
; Picosecond 1:
;  0   1   2   3   4   5   6
; [ ] ( ) ... ... [ ] ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] (S) ... ... [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [S]             [S]     [S]
;                 [ ]     [ ]
;
;
; Picosecond 2:
;  0   1   2   3   4   5   6
; [ ] [S] (.) ... [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [S]             [S]     [S]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [ ] (.) ... [ ] ... [ ]
; [S] [S]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [S]     [S]
;
;
; Picosecond 3:
;  0   1   2   3   4   5   6
; [ ] [ ] ... (.) [ ] ... [ ]
; [S] [S]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [S]     [S]
;
;  0   1   2   3   4   5   6
; [S] [S] ... (.) [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [ ]             [S]     [S]
;                 [ ]     [ ]
;
;
; Picosecond 4:
;  0   1   2   3   4   5   6
; [S] [S] ... ... ( ) ... [ ]
; [ ] [ ]         [ ]     [ ]
; [ ]             [S]     [S]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... ( ) ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;
; Picosecond 5:
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... [ ] (.) [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [S] ... ... [S] (.) [S]
; [ ] [ ]         [ ]     [ ]
; [S]             [ ]     [ ]
;                 [ ]     [ ]
;
;
; Picosecond 6:
;  0   1   2   3   4   5   6
; [ ] [S] ... ... [S] ... (S)
; [ ] [ ]         [ ]     [ ]
; [S]             [ ]     [ ]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... [ ] ... ( )
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
; In this situation, you are caught in layers 0 and 6, because your packet
;  entered the layer when its scanner was at the top when you entered it.
;  You are not caught in layer 1, since the scanner moved into the top of
;  the layer once you were already there.
;
; The severity of getting caught on a layer is equal to its depth multiplied
;  by its range. (Ignore layers in which you do not get caught.) The severity
;  of the whole trip is the sum of these values. In the example above, the trip
;  severity is 0*3 + 6*4 = 24.
;
; Given the details of the firewall you've recorded, if you leave immediately,
;  what is the severity of your whole trip? (Answer: 1844)
;

(struct scanner (rng idx dir) #:transparent #:mutable)

(define (one-picosecond firewall)
  (for ((ptr (hash-keys firewall)))
       (let* ((scan-rec (hash-ref firewall ptr))
              (rng-lmt (sub1 (scanner-rng scan-rec)))
              (cur-dir (scanner-dir scan-rec))
              (cur-idx (scanner-idx scan-rec)))
         (cond
           ((and (eqv? cur-dir 'up) (zero? cur-idx))
            (hash-set! firewall ptr (struct-copy scanner scan-rec
                                                 (dir 'down)
                                                 (idx (add1 cur-idx)))))
           ((and (eqv? cur-dir 'down) (= rng-lmt cur-idx))
            (hash-set! firewall ptr (struct-copy scanner scan-rec
                                                 (dir 'up)
                                                 (idx (sub1 cur-idx)))))
           (else
             (hash-set! firewall ptr (struct-copy scanner scan-rec
                                                  (idx (if (eqv? cur-dir 'down)
                                                           (add1 cur-idx)
                                                           (sub1 cur-idx))))))))))

(define (build-firewall filename)
  (let ((lines (file->lines filename))
        (firewall (make-hash))
        (packet-idx -1)
        (severity 0)
        (max-depth 0)
        (pause 0)
        (pico-cnt 0)
        (caught #f))
    (for ((line lines))
         (define-values (depth range) (apply values (map string->number (regexp-match* #rx"([0-9]+)" line))))
         (hash-set! firewall depth (scanner range 0 'down)))
    (set! max-depth (apply max (hash-keys firewall)))
    (lambda (cmd . args)
      (case cmd
        ((show) (format "packet-idx ~a, pico: ~a, severity: ~a, firewall: ~a" packet-idx pico-cnt severity firewall))

        ((position) packet-idx)

        ((max-depth) max-depth)

        ((caught) caught)

        ((reset)
         (for ((key (hash-keys firewall)))
              (hash-set! firewall key (struct-copy scanner (hash-ref firewall key) (idx 0) (dir 'down))))
         (set! packet-idx -1)
         (set! severity 0)
         (set! pause 0)
         (set! pico-cnt 0)
         (set! caught #f))

        ((delay) (set! pause (car args)))

        ((step)
         (set! pico-cnt (add1 pico-cnt))
         (if (> pause 0)
             (set! pause (sub1 pause))
             (begin
               (set! packet-idx (add1 packet-idx))

               (when (> packet-idx max-depth)
                 (printf ".")
                 ;                 (printf "Hi Edward ZZZ, reached max-depth, severity: ~a~n" severity)
                 ;                 (error (format "Reached max depth, severity: ~a~n" severity))
                 )

               (when (hash-has-key? firewall packet-idx)
                 (when (zero? (scanner-idx (hash-ref firewall packet-idx)))
                   (set! caught #t)
                   (set! severity (+ severity (* packet-idx (scanner-rng (hash-ref firewall packet-idx)))))))))
         (one-picosecond firewall))
        (else "Unknown command '~a' for firewall.~n" cmd)))))

(define (part-one filename)
  (let ((firewall (build-firewall filename)))
   (let loop ((i 0))
    (firewall 'step)
    (loop (add1 i))))) ; Runs till error stops it at severity 1844

;
; --- Part Two ---
;
; Now, you need to pass through the firewall without being caught - easier
;  said than done.
;
; You can't control the speed of the packet, but you can delay it any number
;  of picoseconds. For each picosecond you delay the packet before beginning
;  your trip, all security scanners move one step. You're not in the firewall
;  during this time; you don't enter layer 0 until you stop delaying the packet.
;
; In the example above, if you delay 10 picoseconds (picoseconds 0 - 9), you
;  won't get caught:
;
; State after delaying:
;  0   1   2   3   4   5   6
; [ ] [S] ... ... [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [S]             [S]     [S]
;                 [ ]     [ ]
;
; Picosecond 10:
;  0   1   2   3   4   5   6
; ( ) [S] ... ... [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [S]             [S]     [S]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; ( ) [ ] ... ... [ ] ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;
; Picosecond 11:
;  0   1   2   3   4   5   6
; [ ] ( ) ... ... [ ] ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [S] (S) ... ... [S] ... [S]
; [ ] [ ]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;
; Picosecond 12:
;  0   1   2   3   4   5   6
; [S] [S] (.) ... [S] ... [S]
; [ ] [ ]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [ ] (.) ... [ ] ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;
; Picosecond 13:
;  0   1   2   3   4   5   6
; [ ] [ ] ... (.) [ ] ... [ ]
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [S] ... (.) [ ] ... [ ]
; [ ] [ ]         [ ]     [ ]
; [S]             [S]     [S]
;                 [ ]     [ ]
;
;
; Picosecond 14:
;  0   1   2   3   4   5   6
; [ ] [S] ... ... ( ) ... [ ]
; [ ] [ ]         [ ]     [ ]
; [S]             [S]     [S]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... ( ) ... [ ]
; [S] [S]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [S]     [S]
;
;
; Picosecond 15:
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... [ ] (.) [ ]
; [S] [S]         [ ]     [ ]
; [ ]             [ ]     [ ]
;                 [S]     [S]
;
;  0   1   2   3   4   5   6
; [S] [S] ... ... [ ] (.) [ ]
; [ ] [ ]         [ ]     [ ]
; [ ]             [S]     [S]
;                 [ ]     [ ]
;
;
; Picosecond 16:
;  0   1   2   3   4   5   6
; [S] [S] ... ... [ ] ... ( )
; [ ] [ ]         [ ]     [ ]
; [ ]             [S]     [S]
;                 [ ]     [ ]
;
;  0   1   2   3   4   5   6
; [ ] [ ] ... ... [ ] ... ( )
; [S] [S]         [S]     [S]
; [ ]             [ ]     [ ]
;                 [ ]     [ ]
;
; Because all smaller delays would get you caught, the fewest number of
;  picoseconds you would need to delay to get through safely is 10.
;
; What is the fewest number of picoseconds that you need to delay the
;  packet to pass through the firewall without being caught?
;  (Answer: 3897604)
;

(define (part-two-broke filename)
  (let* ((firewall (build-firewall filename))
         (max-p-idx (firewall 'max-depth)))
    (let loop ((pause 0))
     (firewall 'reset)
     (firewall 'delay pause)
     (let loop2 ((p-index (firewall 'position)))
      (if (> p-index max-p-idx)
          pause
          (begin
            (firewall 'step)
            (loop2 (firewall 'position)))))
     (if (not (firewall 'caught))
         pause
         (loop (add1 pause))))))

; (check-equal? (part-two-broke "test-data.txt") 10)

; Well, that took hours and hours and came up with nothing. :-(
; I'm going to try a more mathematical way to solve this. (sort of)

(define (depths depth)
  (generator ()
             (let ((direction 'down)
                   (cur-num 0))
               (let loop ((i 0))
                (yield cur-num)
                (if (eqv? direction 'down)
                    (set! cur-num (add1 cur-num))
                    (set! cur-num (sub1 cur-num)))
                (when (and (= cur-num (sub1 depth)) (eqv? direction 'down)) (set! direction 'up))
                (when (and (zero? cur-num) (eqv? direction 'up)) (set! direction 'down))
                (loop (add1 i))))))

(define (part-two filename)
  (let ((lines (file->lines filename))
        (firewall (make-hash))
        (status (make-hash)))

    ; Read in the lines into the firewall hash as generators
    (for ((line lines))
         (define-values (depth range) (apply values (map string->number (regexp-match* #rx"([0-9]+)" line))))
         (hash-set! firewall depth (depths range)))

    ; Advance the generators by their depth
    (for ((depth (hash-keys firewall)))
         (for ((i (range depth))) ((hash-ref firewall depth))))

    ; Advance all generators together by picosecond
    (let loop ((picosecond 0))
     (for ((depth (hash-keys firewall)))
          (hash-set! status depth ((hash-ref firewall depth))))

     ; If All values in status are NOT zero, then we have a winner
     (when (> (apply min (hash-values status)) 0)
       (printf "Picosecond: ~a, Status: ~a~n" picosecond (hash-values status)))

     ; Only do 100 lines for now...
;     (when (< picosecond 1000000) (loop (add1 picosecond)))
     (loop (add1 picosecond)))))

(part-two "day13-data.txt") ; -> 3897604

