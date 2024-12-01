
(require rackunit)
(require racket/trace)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 11: Radioisotope Thermoelectric Generators ---
;
; You come upon a column of four floors that have been entirely sealed off from
;  the rest of the building except for a small dedicated lobby. There are some
;  radiation warnings and a big sign which reads "Radioisotope Testing Facility".
;
; According to the project status board, this facility is currently being used to
;  experiment with Radioisotope Thermoelectric Generators (RTGs, or simply "generators")
;  that are designed to be paired with specially-constructed microchips. Basically,
;  an RTG is a highly radioactive rock that generates electricity through heat.
;
; The experimental RTGs have poor radiation containment, so they're dangerously
;  radioactive. The chips are prototypes and don't have normal radiation shielding,
;  but they do have the ability to generate an electromagnetic radiation shield when
;  powered. Unfortunately, they can only be powered by their corresponding RTG. An
;  RTG powering a microchip is still dangerous to other microchips.
;
; In other words, if a chip is ever left in the same area as another RTG, and it's
;  not connected to its own RTG, the chip will be fried. Therefore, it is assumed
;  that you will follow procedure and keep chips connected to their corresponding
;  RTG when they're in the same room, and away from other RTGs otherwise.
;
; These microchips sound very interesting and useful to your current activities,
;  and you'd like to try to retrieve them. The fourth floor of the facility has
;  an assembling machine which can make a self-contained, shielded computer for
;  you to take with you - that is, if you can bring it all of the RTGs and microchips.
;
; Within the radiation-shielded part of the facility (in which it's safe to have
;  these pre-assembly RTGs), there is an elevator that can move between the four
;  floors. Its capacity rating means it can carry at most yourself and two RTGs or
;  microchips in any combination. (They're rigged to some heavy diagnostic equipment
;  - the assembling machine will detach it for you.) As a security measure, the
;  elevator will only function if it contains at least one RTG or microchip. The
;  elevator always stops on each floor to recharge, and this takes long enough that
;  the items within it and the items on that floor can irradiate each other. (You
;  can prevent this if a Microchip and its Generator end up on the same floor in
;  this way, as they can be connected while the elevator is recharging.)
;
; You make some notes of the locations of each component of interest (your puzzle
;  input). Before you don a hazmat suit and start moving things around, you'd like
;  to have an idea of what you need to do.
;
; When you enter the containment area, you and the elevator will start on the first floor.
;
; For example, suppose the isolated area has the following arrangement:
;
; The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
; The second floor contains a hydrogen generator.
; The third floor contains a lithium generator.
; The fourth floor contains nothing relevant.
;
; As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M
;  for Microchip, and G for Generator), the initial state looks like this:
;
; F4 .  .  .  .  .  
; F3 .  .  .  LG .  
; F2 .  HG .  .  .  
; F1 E  .  HM .  LM 
;
; Then, to get everything up to the assembling machine on the fourth floor, the
;  following steps could be taken:
;
; Bring the Hydrogen-compatible Microchip to the second floor, which is safe because
;  it can get power from the Hydrogen Generator:
;
; F4 .  .  .  .  .  
; F3 .  .  .  LG .  
; F2 E  HG HM .  .  
; F1 .  .  .  .  LM 
;
; Bring both Hydrogen-related items to the third floor, which is safe because the
;  Hydrogen-compatible microchip is getting power from its generator:
;
; F4 .  .  .  .  .  
; F3 E  HG HM LG .  
; F2 .  .  .  .  .  
; F1 .  .  .  .  LM 
;
; Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible
;  Microchip back down with you so you can still use the elevator:
;
; F4 .  .  .  .  .  
; F3 .  HG .  LG .  
; F2 E  .  HM .  .  
; F1 .  .  .  .  LM 
;
; At the first floor, grab the Lithium-compatible Microchip, which is safe because
;  Microchips don't affect each other:
;
; F4 .  .  .  .  .  
; F3 .  HG .  LG .  
; F2 .  .  .  .  .  
; F1 E  .  HM .  LM 
;
; Bring both Microchips up one floor, where there is nothing to fry them:
;
; F4 .  .  .  .  .  
; F3 .  HG .  LG .  
; F2 E  .  HM .  LM 
; F1 .  .  .  .  .  
;
; Bring both Microchips up again to floor three, where they can be temporarily
;  connected to their corresponding generators while the elevator recharges,
;  preventing either of them from being fried:
;
; F4 .  .  .  .  .  
; F3 E  HG HM LG LM 
; F2 .  .  .  .  .  
; F1 .  .  .  .  .  
;
; Bring both Microchips to the fourth floor:
;
; F4 E  .  HM .  LM 
; F3 .  HG .  LG .  
; F2 .  .  .  .  .  
; F1 .  .  .  .  .  
;
; Leave the Lithium-compatible microchip on the fourth floor, but bring the
;  Hydrogen-compatible one so you can still use the elevator; this is safe because
;  although the Lithium Generator is on the destination floor, you can connect
;  Hydrogen-compatible microchip to the Hydrogen Generator there:
;
; F4 .  .  .  .  LM 
; F3 E  HG HM LG .  
; F2 .  .  .  .  .  
; F1 .  .  .  .  .  
;
; Bring both Generators up to the fourth floor, which is safe because you can
;  connect the Lithium-compatible Microchip to the Lithium Generator upon arrival:
;
; F4 E  HG .  LG LM 
; F3 .  .  HM .  .  
; F2 .  .  .  .  .  
; F1 .  .  .  .  .  
;
; Bring the Lithium Microchip with you to the third floor so you can use the elevator:
;
; F4 .  HG .  LG .  
; F3 E  .  HM .  LM 
; F2 .  .  .  .  .  
; F1 .  .  .  .  .  
;
; Bring both Microchips to the fourth floor:
;
; F4 E  HG HM LG LM 
; F3 .  .  .  .  .  
; F2 .  .  .  .  .  
; F1 .  .  .  .  .  
;
; In this arrangement, it takes 11 steps to collect all of the objects at the fourth
;  floor for assembly. (Each elevator stop counts as one step, even if nothing is
;  added to or removed from it.)
;
; In your situation, what is the minimum number of steps required to bring all of
;  the objects to the fourth floor? (Answer: 31) !! Totally Cheated from Reddit!!
;

(struct state (elev floors) #:transparent)

(define (elev-options cur-state)
  (let* ((fl (state-elev cur-state))
         (floors (state-floors cur-state))
         (fl0-cnt (length (vector-ref floors 0)))
         (fl1-cnt (length (vector-ref floors 1)))
         (fl2-cnt (length (vector-ref floors 2))))
    (cond
      ((= fl 0) '(1))
      ((and (= fl 1) (zero? fl0-cnt)) '(2))
      ((= fl 1) '(0 2))
      ((and (= fl 2) (zero? fl0-cnt) (zero? fl1-cnt)) '(3))
      ((= fl 2) '(1 3))
      ((= fl 3) '(2)))))

(define (elem-regex elem)
  (let ((parts (car (regexp-match* #rx"^(.*?)(.)$" elem #:match-select cdr))))
   (apply values parts)))

(define (floor-bad? floor-lst)
  (let ((bad #f)
        (groups (make-hash)))
    (hash-set! groups "M" '())
    (hash-set! groups "G" '())
    (for ((elem floor-lst))
         (define-values (pre post) (elem-regex elem))

         (when (not (hash-has-key? groups pre)) (hash-set! groups pre '()))
         (when (not (hash-has-key? groups post)) (hash-set! groups post '()))

         (hash-set! groups pre (cons post (hash-ref groups pre)))
         (hash-set! groups post (cons pre (hash-ref groups post))))
    ; Formula: Microchip with Generator that is not it's Element
    ;  * We have both Microchips and Generators:
    ;    * Microchips >= 1 (check), and 
    ;    * Generators >= 1 (check)
    ;  * Microchip Element without same Generator Element:
    ;    * For each Microchip check for Generator of same Element 
    (when (and (> (length (hash-ref groups "M")) 0)
               (> (length (hash-ref groups "G")) 0))
      (for ((m (hash-ref groups "M")))
           (when (not (member m (hash-ref groups "G")))
             (set! bad #t))))
    bad))

(define (valid? state-rec)
  (let ((groups (make-hash))
        (valid #t))
    (for ((f (range 4)))
         (let ((the-floor (vector-ref (state-floors state-rec) f)))
          (cond
            ((null? the-floor) )
            ((= (length the-floor) 1) )
            ((floor-bad? the-floor) (set! valid #f)))))
    valid))

(define (on-floor floor-num state-rec)
  (vector-ref (state-floors state-rec) floor-num))

(define (variations cur-state)
  (let* ((cur-floor (state-elev cur-state))
         (elevs (elev-options cur-state))
         (return '()))
    (for ((floor-to elevs))
         (for ((thing (one-or-two-things (combinations (on-floor cur-floor cur-state)))))
              (define test-rec (make-variation cur-state cur-floor floor-to thing))
              (when (valid? test-rec)
                (set! return (cons test-rec return)))))
    return))

(define (remove-things things to-remove)
  (foldl (lambda (a b) (remove a b)) things to-remove))

(define (add-things things to-add)
  (foldl (lambda (a b) (cons a b)) things to-add))

(define (one-or-two-things combos)
  (filter (lambda (lst) (or (= 1 (length lst)) (= 2 (length lst)))) combos))

(define (make-variation state-rec from-floor to-floor things)
  (struct-copy state state-rec
               (elev to-floor)
               (floors (list->vector
                         (map
                           (lambda (f)
                             (let ((things-on-floor (on-floor f state-rec)))
                              (cond
                                ((= f from-floor) (remove-things things-on-floor things))
                                ((= f to-floor)   (add-things things-on-floor things))
                                (else things-on-floor))))
                           (range 4))))))

; ((HG) (HM) (LG LM) ()) -> HG HM LG LM -> aG aM bG bM
(define (make-generic-key the-state)
  (let* ((floors (state-floors the-state))
         (in-order (flatten (vector->list floors)))
         (lookup (make-hash))
         (cur-char #\a))
    (for ((the-floor floors))
         (for ((thing the-floor))
              (define-values (elem type) (elem-regex thing))
              (if (hash-has-key? lookup elem)
                  (hash-set! lookup thing (string-append (hash-ref lookup elem) type))
                  (begin
                    (hash-set! lookup elem (string cur-char))
                    (hash-set! lookup thing (string-append (string cur-char) type))
                    (set! cur-char (integer->char (add1 (char->integer cur-char))))))))
    (struct-copy state the-state
                 (floors (list->vector
                           (map
                             (lambda (f)
                               (map (lambda(orig) (hash-ref lookup orig)) (on-floor f the-state)))
                             (range 4)))))))

(define (goal? the-state)
  (let ((floors (state-floors the-state)))
   (if (and (zero? (length (vector-ref floors 0)))
            (zero? (length (vector-ref floors 1)))
            (zero? (length (vector-ref floors 2))))
       #t
       #f)))

(define (display-state the-state)
  (let ((heads (vector "F1" "F2" "F3" "F4"))
        (floors (state-floors the-state)))
    (do ((i 3 (sub1 i)))
        ((< i 0) )
        (printf "  ~a " (vector-ref heads i))
        (if (= i (state-elev the-state))
            (printf "E  ")
            (printf ".  "))
        (do ((j 0 (add1 j)))
            ((> j (sub1 elements)) )
            (if (member (vector-ref items j) (vector-ref floors i))
                (printf "~a " (vector-ref items j))
                (printf ".   ")))
        (printf "~n"))
    (printf "~n")))

(define (display-solution solution-list)
  (for ((a-state solution-list))
       (display-state a-state))
  (printf "~nSolution: ~a~n" solution-list)
  (printf "Length of Solution: ~a~n" (length solution-list))
  (error "Done!"))

(define seen (make-hash)) ; Compare floor arrangements for similarity

(define (find-shortest-path state-list (path '()))
  (let ((next-list '()))
   (for ((cur-state state-list))
        (when (not (hash-has-key? seen cur-state))
          (define generic-key (make-generic-key cur-state))
          (when (not (hash-has-key? seen generic-key))
            (hash-set! seen cur-state generic-key)
            (hash-set! seen generic-key cur-state)
            (define from-here-states (variations cur-state))
            (for ((new-state from-here-states))
                 (when (goal? new-state) (display-solution (cons new-state path)))
                 (set! next-list (append from-here-states next-list))))))
   (find-shortest-path next-list
                       (cons cur-state path)
                       
                       )))

(define start (state 0 (vector '("HM" "LM") '("HG") '("LG") '())))
(define items (vector "HG" "HM" "LG" "LM"))
(define elements (vector-length items))
(find-shortest-path (list start))

;(define items (vector "THG" "THM" "PLG" "PLM" "STG" "STM" "PRG" "PRM" "RUG" "RUM"))
;(define elements (vector-length items))
;(define start (state 0 (vector '("THG" "THM" "PLG" "STG")
;                               '("PLM" "STM")
;                               '("PRG" "PRM" "RUG" "RUM")
;                               '())))
;(find-shortest-path (list start)) ; Answer should be 31 ; from reddit :-(

;
; --- Part Two ---
;
; You step into the cleanroom separating the lobby from the isolated area and put on the
;  hazmat suit.
; 
; Upon entering the isolated containment area, however, you notice some extra parts on
;  the first floor that weren't listed on the record outside:
; 
; * An elerium generator.
; * An elerium-compatible microchip.
; * A dilithium generator.
; * A dilithium-compatible microchip.
;
; These work just like the other generators and microchips. You'll have to get them up to
;  assembly as well.
; 
; What is the minimum number of steps required to bring all of the objects, including these
;  four new ones, to the fourth floor?
; 
