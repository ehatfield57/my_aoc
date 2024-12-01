
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 20: Particle Swarm ---
;
; Suddenly, the GPU contacts you, asking for help. Someone has asked it
;  to simulate too many particles, and it won't be able to finish them
;  all in time to render the next frame at this rate.
;
; It transmits to you a buffer (your puzzle input) listing each particle
;  in order (starting with particle 0, then particle 1, particle 2, and
;  so on). For each particle, it provides the X, Y, and Z coordinates for
;  the particle's position (p), velocity (v), and acceleration (a), each
;  in the format <X,Y,Z>.
;
; Each tick, all particles are updated simultaneously. A particle's properties
;  are updated in the following order:
;
; * Increase the X velocity by the X acceleration.
; * Increase the Y velocity by the Y acceleration.
; * Increase the Z velocity by the Z acceleration.
; * Increase the X position by the X velocity.
; * Increase the Y position by the Y velocity.
; * Increase the Z position by the Z velocity.
;
; Because of seemingly tenuous rationale involving z-buffering, the GPU would
;  like to know which particle will stay closest to position <0,0,0> in the
;  long term. Measure this using the Manhattan distance, which in this situation
;  is simply the sum of the absolute values of a particle's X, Y, and Z position.
;
; For example, suppose you are only given two particles, both of which stay
;  entirely on the X-axis (for simplicity). Drawing the current states of
;  particles 0 and 1 (in that order) with an adjacent a number line and
;  diagram of current X positions (marked in parentheses), the following
;  would take place:
;
; p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
; p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)
;
; p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
; p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)
;
; p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
; p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)
;
; p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
; p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)   
;
; At this point, particle 1 will never be closer to <0,0,0> than particle 0,
;  and so, in the long run, particle 0 will stay closest.
;
; Which particle will stay closest to position <0,0,0> in the long term?
;  (Answer: 308)
;

(define (xyz str)
  (map string->number (regexp-match* #rx"([0-9-]+)" str)))

(define (load-particles filename)
  (let ((particles (make-hash))
        (lines (file->lines filename))
        (p-index 0))
    (for ((line lines))
         (define-values (p v a) (apply values (regexp-match* #rx"=<([^>]+)>" line #:match-select cadr)))
         (hash-set! particles p-index (list (xyz p) (xyz v) (xyz a)))
         (set! p-index (add1 p-index)))
    particles))

(define (manhat part)
  (apply + (map abs part)))

(define (tick particle)
  (define-values (px py pz vx vy vz ax ay az) (apply values (flatten particle)))
  (list (list (+ px vx ax) (+ py vy ay) (+ pz vz az))
        (list (+    vx ax) (+    vy ay) (+    vz az))
        (list          ax           ay           az)))

(define (sorted-pairs my-hash)
    (sort (hash->list my-hash) 
          (lambda (pair1 pair2) 
            (< (cdr pair1) (cdr pair2)))))

(define (part-one filename (times 10))
  (let ((particles (load-particles filename))
        (distances (make-hash)))
    (for ((part-idx (hash-keys particles)))
         (hash-set! distances part-idx (list (manhat (car (hash-ref particles part-idx))))))
    (for ((i (range times)))
         (for ((part-idx (hash-keys particles)))
              (let* ((particle (hash-ref particles part-idx))
                     (new-particle (tick particle))
                     (new-manhat (manhat (first new-particle))))
                (hash-set! distances part-idx (cons new-manhat (hash-ref distances part-idx)))
                (hash-set! particles part-idx new-particle))))
    (let ((averages (make-hash)))
     (for ((dist-idx (hash-keys distances)))
          (hash-set! averages dist-idx (exact->inexact (/ (apply + (hash-ref distances dist-idx)) times))))

     (car (first (sorted-pairs averages))))))

(check-equal? (part-one "day20-data.txt" 10) 67)
;(check-equal? (part-one "day20-data.txt" 100) 401)
;(check-equal? (part-one "day20-data.txt" 1000) 308)
;(check-equal? (part-one "day20-data.txt" 10000) 308)

;
; --- Part Two ---
;
; To simplify the problem further, the GPU would like to remove any particles that
;  collide. Particles collide if their positions ever exactly match. Because particles
;  are updated simultaneously, more than two particles can collide at the same time
;  and place. Once particles collide, they are removed and cannot collide with anything
;  else after that tick.
;
; For example:
;
; p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>    
; p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
; p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
; p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>
;
; p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>    
; p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
; p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)   
; p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>
;
; p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>    
; p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
; p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)      
; p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>
;
; ------destroyed by collision------    
; ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
; ------destroyed by collision------                      (3)         
; p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>
;
; In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and
;  place marked X. On the next tick, particle 3 passes through unharmed.
;
; How many particles are left after all collisions are resolved? (Answer: 504)
;

(define (sorted-pairs my-hash)
    (sort (hash->list my-hash) 
          (lambda (pair1 pair2) 
            (< (cdr pair1) (cdr pair2)))))

(define (part-two filename (times 10))
  (let ((particles (load-particles filename))
        (positions (make-hash)))

    ; Do this for a number of times
    (for ((i (range times)))
         ; Fill a hash with all the particle positions
         (hash-clear! positions)
         (for ((key (hash-keys particles)))
              (let* ((particle (hash-ref particles key))
                     (position (car particle)))
                (if (hash-has-key? positions position)
                    (hash-set! positions position (cons key (hash-ref positions position)))
                    (hash-set! positions position (list key)))))

         ; If any positions have more than one particle, then remove them
         (for ((position (hash-keys positions)))
               (when (> (length (hash-ref positions position)) 1)
                 (for ((key (hash-ref positions position)))
                      (hash-remove! particles key))))

         ; tick the particles to their new location
         (for ((key (hash-keys particles)))
              (hash-set! particles key (tick (hash-ref particles key)))))

    (length (hash-keys particles))))

(part-two "day20-data.txt")

(check-equal? (part-two "day20-data.txt" 10) 1000)
(check-equal? (part-two "day20-data.txt" 100) 504)
(check-equal? (part-two "day20-data.txt" 1000) 504)
(check-equal? (part-two "day20-data.txt" 10000) 504)

