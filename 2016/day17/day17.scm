
(require rackunit)
(require file/md5)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 17: Two Steps Forward ---
;
; You're trying to access a secure vault protected by a 4x4 grid of small rooms connected
;  by doors. You start in the top-left room (marked S), and you can access the vault (marked
;  V) once you reach the bottom-right room:
;
; #########
; #S| | | #
; #-#-#-#-#
; # | | | #
; #-#-#-#-#
; # | | | #
; #-#-#-#-#
; # | | |  
; ####### V
;
; Fixed walls are marked with #, and doors are marked with - or |.
;
; The doors in your current room are either open or closed (and locked) based on the hexadecimal
;  MD5 hash of a passcode (your puzzle input) followed by a sequence of uppercase characters
;  representing the path you have taken so far (U for up, D for down, L for left, and R for right).
;
; Only the first four characters of the hash are used; they represent, respectively, the doors up,
;  down, left, and right from your current position. Any b, c, d, e, or f means that the corresponding
;  door is open; any other character (any number or a) means that the corresponding door is closed
;  and locked.
;
; To access the vault, all you need to do is reach the bottom-right room; reaching this room opens
;  the vault and all doors in the maze.
;
; For example, suppose the passcode is hijkl. Initially, you have taken no steps, and so your path is
;  empty: you simply find the MD5 hash of hijkl alone. The first four characters of this hash are ced9,
;  which indicate that up is open (c), down is open (e), left is open (d), and right is closed and locked
;  (9). Because you start in the top-left corner, there are no "up" or "left" doors to be open, so your
;  only choice is down.
;
; Next, having gone only one step (down, or D), you find the hash of hijklD. This produces f2bc, which
;  indicates that you can go back up, left (but that's a wall), or right. Going right means hashing
;  hijklDR to get 5745 - all doors closed and locked. However, going up instead is worthwhile: even
;  though it returns you to the room you started in, your path would then be DU, opening a different
;  set of doors.
;
; After going DU (and then hashing hijklDU to get 528e), only the right door is open; after going DUR, all
;  doors lock. (Fortunately, your actual passcode is not hijkl).
;
; Passcodes actually used by Easter Bunny Vault Security do allow access to the vault if you know the right
;  path. For example:
;
; * If your passcode were ihgpwlah, the shortest path would be DDRRRD.
; * With kglvqrro, the shortest path would be DDUDRLRRUDRD.
; * With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.
;
; Given your vault's passcode, what is the shortest path (the actual path, not just the length) to reach
;  the vault? (Answer: DUDDRLRRRD)
;
; Your puzzle input is gdjjyniy.
;

(define (door-open? c) (if (member c '(#\b #\c #\d #\e #\f)) #t #f))

(define (find-directions md5-str)
  (let ((dirs "UDLR") (path ""))
   (for ((idx (range 4)))
        (when (door-open? (string-ref md5-str idx))
          (set! path (string-append (string (string-ref dirs idx)) path))))
   path))

(define maze (hash  1 (hash "U"  0 "D"  5 "L"  0 "R"  2)
                    2 (hash "U"  0 "D"  6 "L"  1 "R"  3)
                    3 (hash "U"  0 "D"  7 "L"  2 "R"  4)
                    4 (hash "U"  0 "D"  8 "L"  3 "R"  0)
                    5 (hash "U"  1 "D"  9 "L"  0 "R"  6)
                    6 (hash "U"  2 "D" 10 "L"  5 "R"  7)
                    7 (hash "U"  3 "D" 11 "L"  6 "R"  8)
                    8 (hash "U"  4 "D" 12 "L"  7 "R"  0)
                    9 (hash "U"  5 "D" 13 "L"  0 "R" 10)
                   10 (hash "U"  6 "D" 14 "L"  9 "R" 11)
                   11 (hash "U"  7 "D" 15 "L" 10 "R" 12)
                   12 (hash "U"  8 "D" 16 "L" 11 "R"  0)
                   13 (hash "U"  9 "D"  0 "L"  0 "R" 14)
                   14 (hash "U" 10 "D"  0 "L" 13 "R" 15)
                   15 (hash "U" 11 "D"  0 "L" 14 "R" 16)
                   16 (hash "U" 12 "D"  0 "L" 15 "R"  0)))

(define passcode "hijkl")

(define (get-md5 path) (bytes->string/latin-1 (md5 (string-append passcode path))))

(define (available room path)
  (let* ((paths '())
         (the-md5 (get-md5 path))
         (dirs (find-directions the-md5))
         (doors (hash-ref maze room)))
    (for ((dir (hash-keys doors)))
         (when (and (string-contains? dirs dir) (not (zero? (hash-ref doors dir))))
           (set! paths (cons (cons dir (hash-ref doors dir)) paths))))
    paths))


(define all-paths '())

(define (shortest-paths avails)
  (let ((new-paths '()))
   (for ((combo avails))
        (let* ((dir (car combo))
               (room (cdr combo))
               (more (available room dir)))
;         (when (= room 16) (error (format "Shortest solution found: ~a~n" dir))) ; Part One
          (if (= room 16)
              (set! all-paths (cons dir all-paths))
              (begin
                (set! more (map (lambda (d-r) (cons (string-append dir (car d-r)) (cdr d-r))) more))
                (set! new-paths (append more new-paths))))))

   (if (zero? (length new-paths))
       (if (zero? (length all-paths))
           (printf "No solution found for passcode: '~a'~n" passcode)
           (printf "Longest path found for passcode '~a' is ~a~n" passcode (apply max (map string-length all-paths))))
       (shortest-paths new-paths))))

(define (part-one pcode)
  (set! all-paths '())
  (set! passcode pcode)
  (shortest-paths (available 1 "")))

;(part-one "ihgpwlah")
;(part-one "gdjjyniy") ; -> "DUDDRLRRRD" ; Part One solution

; --- Part Two ---
;
; You're curious how robust this security solution really is, and so you decide to
;  find longer and longer paths which still provide access to the vault. You remember
;  that paths always end the first time they reach the bottom-right room (that is,
;  they can never pass through it, only end in it).
;
; For example:
;
; * If your passcode were ihgpwlah, the longest path would take 370 steps.
; * With kglvqrro, the longest path would be 492 steps long.
; * With ulqzkmiv, the longest path would be 830 steps long.
;
; What is the length of the longest path that reaches the vault? (Answer: ?)
;

(define (validate solution (room 1))
  (when (zero? room) (error "Trying to go through a wall!"))
  (printf "Hi Edward, room: ~a, solution: ~a~n" room solution)
  (let* ((first-chr (string (string-ref solution 0)))
         (rest-sol  (substring solution 1))
         (doors     (hash-ref maze room))
         (to-room   (hash-ref doors first-chr)))
    (cond
      ((string=? "" rest-sol)
       (if (= 16 to-room)
           (printf "Verified!~n")
           (printf "Not verified.~n")))
      (else (validate rest-sol to-room)))))

(define (longest-path avails) ; Part Two?
  (for ((combo avails))
       (let* ((dir (car combo))
              (room (cdr combo))
              (more (available room dir)))
         (if (= room 16)
             (set! all-paths (cons dir all-paths))
             (begin
               (set! more (map (lambda (d-r) (cons (string-append dir (car d-r)) (cdr d-r))) more))
               (longest-path more))))))

(define (part-two pcode)
  (set! all-paths '())
  (set! passcode pcode)
  (longest-path (available 1 "")))

(part-two "gdjjyniy")
(printf "Shortest path: ~a, Longest path: ~a~n"
        (apply min (map string-length all-paths))
        (apply max (map string-length all-paths)))

