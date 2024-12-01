
(require rackunit)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 14: Reindeer Olympics ---
;
; This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must
;  rest occasionally to recover their energy. Santa would like to know which of
;  his reindeer is fastest, and so he has them race.
; 
; Reindeer can only either be flying (always at their top speed) or resting (not
;  moving at all), and always spend whole seconds in either state.
; 
; For example, suppose you have the following Reindeer:
; 
; * Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
;
; * Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
;
; After one second, Comet has gone 14 km, while Dancer has gone 16 km. After ten seconds,
;  Comet has gone 140 km, while Dancer has gone 160 km. On the eleventh second,
;  Comet begins resting (staying at 140 km), and Dancer continues on for a total distance
;  of 176 km. On the 12th second, both reindeer are resting. They continue to rest until
;  the 138th second, when Comet flies for another ten seconds. On the 174th second,
;  Dancer flies for another 11 seconds.
; 
; In this example, after the 1000th second, both reindeer are resting, and Comet is in
;  the lead at 1120 km (poor Dancer has only gotten 1056 km by that point). So, in this
;  situation, Comet would win (if the race ended at 1000 seconds).
; 
; Given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds,
;  what distance has the winning reindeer traveled?  (Answer: 2696)
;

(struct reindeer (name speed fly-seconds rest-seconds) #:transparent)

(define (make-reindeer a-reindeer)
  (let ((whom a-reindeer)
        (distance 0)
        (countdown (reindeer-fly-seconds a-reindeer))
        (state 'fly))

    (lambda (action . args)
      (cond ((eq? action 'tick)

             (when (zero? countdown)
               (cond
                 ((eqv? state 'fly)
                  (set! state 'rest)
                  (set! countdown (reindeer-rest-seconds whom)))
                 (else
                   (set! state 'fly)
                   (set! countdown (reindeer-fly-seconds whom)))))

             (when (eqv? state 'fly)
               (set! distance (+ (reindeer-speed whom) distance)))

;             (printf "Tick for ~a, second ~a, state: ~a, distance: ~a, countdown: ~a~n"
;                     (~a (reindeer-name whom) #:min-width 7 #:align 'left #:left-pad-string " ")
;                     (~a (car args) #:min-width 5 #:align 'right #:left-pad-string " ")
;                     state distance countdown)

             (set! countdown (sub1 countdown)))

            ((eq? action 'show)
             (printf "~nShowing reindeer for ~a: ~a~n" (reindeer-name whom) whom)
             (printf "  State: ~a, Distance: ~a, Countdown ~a~n" state distance countdown))

            ((eq? action 'who) (reindeer-name whom))
            
            ((eq? action 'distance) distance)

            ((eq? action 'reset) (set! distance 0))

            (else (error (printf "Invalid action: ~a" action)))))))

(define (get-data filename)
  (let ((stable '()))
   (call-with-input-file
     filename
     (lambda (in-port)
       (do ((line (read-line in-port 'any)
                  (read-line in-port 'any)))
           ((eof-object? line))
           (let ((parts (regexp-match* #rx"^([^ ]+) can fly ([0-9]+) .* ([0-9]+) .*?([0-9]+)" line #:match-select values)))
             (define-values (name speed fly rest) (apply values (cdr (car parts))))
             (set! stable
               (cons (make-reindeer
                       (reindeer name (string->number speed) (string->number fly) (string->number rest))
                       ) stable))))))
   stable))

(define (part-one filename seconds)
  (let ((stable (get-data filename)))
   (for ((sec seconds))
        (for-each (lambda (r) (r 'tick (add1 sec))) stable))
;   (for-each (lambda (r) (r 'show)) stable)
   (apply max (map (lambda (r) (r 'distance)) stable))))

(check-equal? (part-one "ed-data.txt" 1000) 1000)
(check-equal? (part-one "test-data.txt" 1000) 1120)
(check-equal? (part-one "day14-data.txt" 2503) 2696)


; --- Part Two ---
;
; Seeing how reindeer move in bursts, Santa decides he's not pleased with the old
;  scoring system.
; 
; Instead, at the end of each second, he awards one point to the reindeer currently
;  in the lead. (If there are multiple reindeer tied for the lead, they each get one
;  point.) He keeps the traditional 2503 second time limit, of course, as doing
;  otherwise would be entirely ridiculous.
; 
; Given the example reindeer from above, after the first second, Dancer is in the
;  lead and gets one point. He stays in the lead until several seconds into Comet's
;  second burst: after the 140th second, Comet pulls into the lead and gets his
;  first point. Of course, since Dancer had been in the lead for the 139 seconds
;  before that, he has accumulated 139 points by the 140th second.
; 
; After the 1000th second, Dancer has accumulated 689 points, while poor Comet, our
;  old champion, only has 312. So, with the new scoring system, Dancer would win
;  (if the race ended at 1000 seconds).
; 
; Again given the descriptions of each reindeer (in your puzzle input), after exactly
;  2503 seconds, how many points does the winning reindeer have?  (Answer: 1084)
;

(define (find-winner-names stable)
  (let* ((max-dist (apply max (map (lambda (r) (r 'distance)) stable)))
         (winners '()))
   (for-each (lambda (r)
               (when (= max-dist (r 'distance))
                 (set! winners (cons (r 'who) winners))))
             stable)
   winners))

(define (part-two filename seconds)
  (let* ((points (make-hash))
         (stable (get-data filename))
         (names (map (lambda (r) (r 'who)) stable)))
    (for-each (lambda (name) (hash-set! points name 0)) names)
    (for ((sec seconds))
         (for-each (lambda (r) (r 'tick (add1 sec))) stable)
         (for-each (lambda (name) (hash-update! points name add1)) (find-winner-names stable)))
    (displayln points)
    (apply max (hash-values points))))

(check-equal? (part-two "test-data.txt" 1000) 689)
(check-equal? (part-two "day14-data.txt" 2503) 1084)

