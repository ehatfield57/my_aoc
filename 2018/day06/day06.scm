
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 6: Chronal Coordinates ---
;
; The device on your wrist beeps several times, and once again you feel
;  like you're falling.
;
; "Situation critical," the device announces. "Destination indeterminate.
;  Chronal interference detected. Please specify new target coordinates."
;
; The device then produces a list of coordinates (your puzzle input). Are
;  they places it thinks are safe or dangerous? It recommends you check
;  manual page 729. The Elves did not give you a manual.
;
; If they're dangerous, maybe you can minimize the danger by finding the
;  coordinate that gives the largest distance from the other points.
;
; Using only the Manhattan distance, determine the area around each coordinate
;  by counting the number of integer X,Y locations that are closest to that
;  coordinate (and aren't tied in distance to any other coordinate).
;
; Your goal is to find the size of the largest area that isn't infinite. For
;  example, consider the following list of coordinates:
;
; 1, 1
; 1, 6
; 8, 3
; 3, 4
; 5, 5
; 8, 9
;
; If we name these coordinates A through F, we can draw them on a grid, putting
;  0,0 at the top left:
;
; ..........
; .A........
; ..........
; ........C.
; ...D......
; .....E....
; .B........
; ..........
; ..........
; ........F.
;
; This view is partial - the actual grid extends infinitely in all directions.
;  Using the Manhattan distance, each location's closest coordinate can be determined,
;  shown here in lowercase:
;
; aaaaa.cccc
; aAaaa.cccc
; aaaddecccc
; aadddeccCc
; ..dDdeeccc
; bb.deEeecc
; bBb.eeee..
; bbb.eeefff
; bbb.eeffff
; bbb.ffffFf
;
; Locations shown as . are equally far from two or more coordinates, and so they
;  don't count as being closest to any.
;
; In this example, the areas of coordinates A, B, C, and F are infinite - while
;  not shown here, their areas extend forever outside the visible grid. However,
;  the areas of coordinates D and E are finite: D is closest to 9 locations, and
;  E is closest to 17 (both including the coordinate's location itself). Therefore,
;  in this example, the size of the largest area is 17.
;
; What is the size of the largest area that isn't infinite? (Answer: 3420)
;

(define (get-points filename)
  (let ((lines (file->lines filename))
        (points (make-hash))
        (alphabet (list->vector (map string (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))
        (index 0))
   (for ((line lines))
        (define-values (x y) (apply values (map string->number (regexp-match* #rx"([0-9]+)" line))))
        (hash-set! points (cons x y) (vector-ref alphabet index))
        (set! index (add1 index)))
   points))

(define (get-size points)
  (let ((min-x 9999)
        (min-y 9999)
        (max-x 0)
        (max-y 0))
    (for ((coor (hash-keys points)))
         (let ((x (car coor))
               (y (cdr coor)))
           (when (< x min-x) (set! min-x x))
           (when (> x max-x) (set! max-x x))
           (when (< y min-y) (set! min-y y))
           (when (> y max-y) (set! max-y y))))
    (values min-x min-y max-x max-y)))

(define (draw-points points (zoom 2))
  (define-values (min-x min-y max-x max-y) (get-size points))
  (for ((y (range (- min-y zoom) (+ zoom max-y))))
       (for ((x (range (- min-x zoom) (+ zoom max-x))))
            (let ((coor (cons x y)))
             (printf "~a" (cond
                            ((and (hash-has-key? points coor) (string=? (hash-ref points coor) ".")) ".")
                            ((hash-has-key? points coor) (hash-ref points coor))
                            (else "-")))))
       (printf "~n"))
  (printf "~n"))

(define (manhattan-distance point x y)
  (+ (abs (- (car point) x)) (abs (- (cdr point) y))))

(define (compare points (zoom 2))
  (let ((distances (make-hash)))
   (define-values (min-x min-y max-x max-y) (get-size points))
   (for ((y (range (- min-y zoom) (+ zoom max-y))))
        (for ((x (range (- min-x zoom) (+ zoom max-x))))
             (let ((closest '())
                   (closest-val 99999)) 
               (for ((point (hash-keys points)))
                    (let ((dist (manhattan-distance point x y)))
                     (cond
                       ((or (zero? (length closest)) (< dist closest-val))
                           (set! closest (list (hash-ref points point)))
                           (set! closest-val dist))
                       ((<= dist closest-val)
                           (set! closest (cons (hash-ref points point) closest))))))
               (hash-set! distances (cons x y) closest))))
   distances))

(define (tally distances)
  (let ((counts (make-hash)))
   (for ((coor (hash-keys distances)))
        (let ((point-list (hash-ref distances coor)))
         (when (= (length point-list) 1)
           (if (hash-has-key? counts (car point-list))
               (hash-set! counts (car point-list) (add1 (hash-ref counts (car point-list))))
               (hash-set! counts (car point-list) 1)))))
   counts))

(define (part-one filename (iters 5))
  (let ((points (get-points filename)))
   (draw-points points)))

; 3222 is too low  :-(
; 3435 is too high :-(
; 3420 is just right! :-D

;
; --- Part Two ---
;
; On the other hand, if the coordinates are safe, maybe the best you can do
;  is try to find a region near as many coordinates as possible.
;
; For example, suppose you want the sum of the Manhattan distance to all of
;  the coordinates to be less than 32. For each location, add up the distances
;  to all of the given coordinates; if the total of those distances is less
;  than 32, that location is within the desired region. Using the same coordinates
;  as above, the resulting region looks like this:
;
; ..........
; .A........
; ..........
; ...###..C.
; ..#D###...
; ..###E#...
; .B.###....
; ..........
; ..........
; ........F.
;
; In particular, consider the highlighted location 4,3 located at the top middle
;  of the region. Its calculation is as follows, where abs() is the absolute
;  value function:
;
; * Distance to coordinate A: abs(4-1) + abs(3-1) =  5
; * Distance to coordinate B: abs(4-1) + abs(3-6) =  6
; * Distance to coordinate C: abs(4-8) + abs(3-3) =  4
; * Distance to coordinate D: abs(4-3) + abs(3-4) =  2
; * Distance to coordinate E: abs(4-5) + abs(3-5) =  3
; * Distance to coordinate F: abs(4-8) + abs(3-9) = 10
; * Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30
;
; Because the total distance to all coordinates (30) is less than 32, the
;  location is within the region.
;
; This region, which also includes coordinates D and E, has a total size
;  of 16.
;
; Your actual region will need to be much larger than this example, though,
;  instead including all locations with a total distance of less than 10000.
;
; What is the size of the region containing all locations which have a total
;  distance to all given coordinates of less than 10000? (Answer: 46667)
;

(define (safe-dist points (less-than 32))
  (let ((safe-distances (make-hash)))
   (define-values (min-x min-y max-x max-y) (get-size points))
   (for ((y (range min-y max-y)))
        (for ((x (range min-x max-x)))
             (let ((distances '()))
              (for ((point (hash-keys points)))
                   (let ((dist (manhattan-distance point x y)))
                    (set! distances (cons dist distances))))
              (when (< (apply + distances) less-than)
                (hash-set! safe-distances (cons x y) #t)))))
   safe-distances))

(define (draw-safe points safe-distances)
  (define-values (min-x min-y max-x max-y) (get-size points))
  (for ((y (range min-y max-y)))
       (for ((x (range min-x max-x)))
            (let ((coor (cons x y)))
             (cond
               ((hash-has-key? points coor) (printf "~a" (hash-ref points coor)))
               ((hash-has-key? safe-distances coor) (printf "#"))
               (else (printf ".")))
             ))
       (printf "~n"))
  (printf "~n"))

(define (part-two filename (less-than 32))
  (let* ((points (get-points filename))
         (safe-distances (safe-dist points less-than)))
    (draw-safe points safe-distances)
    (length (hash-keys safe-distances)))) ; 46667

