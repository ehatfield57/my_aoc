
(require rackunit)

;
; Advent of Code - 2015
;
; Part One:
;
; --- Day 9: All in a Single Night ---
;
; Every year, Santa manages to deliver all of his presents in a single night.
;
; This year, however, he has some new locations to visit; his elves have provided
;  him the distances between every pair of locations. He can start and end at any
;  two (different) locations he wants, but he must visit each location exactly once.
;  What is the shortest distance he can travel to achieve this?  (Answer: ?)
;
; For example, given the following distances:
;
; London to Dublin = 464
; London to Belfast = 518
; Dublin to Belfast = 141
;
; The possible routes are therefore:
;
; Dublin -> London -> Belfast = 982
; London -> Dublin -> Belfast = 605
; London -> Belfast -> Dublin = 659
; Dublin -> Belfast -> London = 659
; Belfast -> Dublin -> London = 605
; Belfast -> London -> Dublin = 982
;
; The shortest of these is London -> Dublin -> Belfast = 605, and so the answer is
;  605 in this example.
;
; What is the distance of the shortest route?  (Answer: 117)
;

(struct edge (city1 city2 dist) #:transparent)

(define (get-data filename)
  (let ((data '()))
   (define in-port (open-input-file filename))
   (with-input-from-file
     filename
     (lambda ()
       (for ([line (in-lines)])
         (let* ((main-parts (regexp-match #rx"^(.*?) to (.*?) = ([0-9]+)$" line))
                 (city1 (cadr main-parts))
                 (city2 (caddr main-parts))
                 (dist (string->number (cadddr main-parts))))
           (set! data (cons (edge city1 city2 dist) data))))))
   (close-input-port in-port)
   data))

(define (shortest-route order cities)
  (let* ((visited '())
         (shortest '())
         (counter (make-hash))
         (sorted (sort cities (lambda (a b) (order (edge-dist a) (edge-dist b)))))
         (get-counter (lambda (key) (hash-ref counter key 0))))
    (for-each
      (lambda (item)
        (let* ((city1 (edge-city1 item))
               (city2 (edge-city2 item))
               (city1-visited (if (member city1 visited) #t #f))
               (city2-visited (if (member city2 visited) #t #f))
               (both-visited  (and city1-visited city2-visited)))

          (when (and (< (get-counter city1) 2) (< (get-counter city2) 2))
            (hash-set! counter city1 (add1 (get-counter city1)))
            (hash-set! counter city2 (add1 (get-counter city2)))

            (set! shortest (cons item shortest))
            (when (not both-visited)
              (if city1-visited
                  (set! visited (cons city2 visited))
                  (set! visited (cons city1 visited)))))))
      sorted)
    (apply + (map edge-dist (reverse (cdr shortest))))))

(check-equal? 605 (shortest-route < (get-data "test-data.txt")))
(check-equal? 117 (shortest-route < (get-data "day9-data.txt")))

; --- Part Two ---
;
; The next year, just to show off, Santa decides to take the route with the longest distance instead.
;
; He can still start and end at any two (different) locations he wants, and he
;  still must visit each location exactly once.
;
; For example, given the distances above, the longest route would be 982 via
;  (for example) Dublin -> London -> Belfast.
;
; What is the distance of the longest route? (Answer: 909)

(check-equal? 982 (shortest-route > (get-data "test-data.txt")))
(check-equal? 909 (shortest-route > (get-data "day9-data.txt")))

