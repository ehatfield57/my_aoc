
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 25: Four-Dimensional Adventure ---
;
; The reindeer's symptoms are getting worse, and neither you nor the
;  white-bearded man have a solution. At least the reindeer has a warm
;  place to rest: a small bed near where you're sitting.
;
; As you reach down, the reindeer looks up at you, accidentally bumping
;  a button on your wrist-mounted device with its nose in the process -
;  a button labeled "help".
;
; "Hello, and welcome to the Time Travel Support Hotline! If you are lost
;  in time and space, press 1. If you are trapped in a time paradox, press
;  2. If you need help caring for a sick reindeer, press 3. If you--"
;
; Beep.
;
; A few seconds later, you hear a new voice. "Hello; please state the nature
;  of your reindeer." You try to describe the situation.
;
; "Just a moment, I think I can remotely run a diagnostic scan." A beam of
;  light projects from the device and sweeps over the reindeer a few times.
;
; "Okay, it looks like your reindeer is very low on magical energy; it should
;  fully recover if we can fix that. Let me check your timeline for a source....
;  Got one. There's actually a powerful source of magical energy about 1000
;  years forward from you, and at roughly your position, too! It looks like...
;  hot chocolate? Anyway, you should be able to travel there to pick some up;
;  just don't forget a mug! Is there anything else I can help you with today?"
;
; You explain that your device isn't capable of going forward in time. "I... see.
;  That's tricky. Well, according to this information, your device should have
;  the necessary hardware to open a small portal and send some hot chocolate back
;  to you. You'll need a list of fixed points in spacetime; I'm transmitting it
;  to you now."
;
; "You just need to align your device to the constellations of fixed points so
;  that it can lock on to the destination and open the portal. Let me look up
;  how much hot chocolate that breed of reindeer needs."
;
; "It says here that your particular reindeer is-- this can't be right, it says
;  there's only one like that in the universe! But THAT means that you're--" You
;  disconnect the call.
;
; The list of fixed points in spacetime (your puzzle input) is a set of four-
;  dimensional coordinates. To align your device, acquire the hot chocolate, and
;  save the reindeer, you just need to find the number of constellations of points
;  in the list.
;
; Two points are in the same constellation if their manhattan distance apart is no
;  more than 3 or if they can form a chain of points, each a manhattan distance no
;  more than 3 from the last, between the two of them. (That is, if a point is close
;  enough to a constellation, it "joins" that constellation.) For example:
;
;  0,0,0,0
;  3,0,0,0
;  0,3,0,0
;  0,0,3,0
;  0,0,0,3
;  0,0,0,6
;  9,0,0,0
; 12,0,0,0
;
; In the above list, the first six points form a single constellation: 0,0,0,0 is
;  exactly distance 3 from the next four, and the point at 0,0,0,6 is connected to
;  the others by being 3 away from 0,0,0,3, which is already in the constellation.
;  The bottom two points, 9,0,0,0 and 12,0,0,0 are in a separate constellation
;  because no point is close enough to connect them to the first constellation. So,
;  in the above list, the number of constellations is 2. (If a point at 6,0,0,0 were
;  present, it would connect 3,0,0,0 and 9,0,0,0, merging all of the points into a
;  single giant constellation instead.)
;
; In this example, the number of constellations is 4:
;
; -1,2,2,0
; 0,0,2,-2
; 0,0,0,-2
; -1,2,0,0
; -2,-2,-2,2
; 3,0,2,-1
; -1,3,2,2
; -1,0,-1,0
; 0,2,1,-2
; 3,0,0,0
;
; In this one, it's 3:
;
; 1,-1,0,1
; 2,0,-1,0
; 3,2,-1,0
; 0,0,3,1
; 0,0,-1,-1
; 2,3,-2,0
; -2,2,0,0
; 2,-2,0,-1
; 1,-1,0,-1
; 3,2,0,2
;
; Finally, in this one, it's 8:
;
; 1,-1,-1,-2
; -2,-2,0,1
; 0,2,1,3
; -2,3,-2,1
; 0,2,3,-2
; -1,-1,1,-2
; 0,-2,-1,0
; -2,2,3,-1
; 1,2,2,0
; -1,-2,0,-2
;
; The portly man nervously strokes his white beard. It's time to get that hot chocolate.
;
; How many constellations are formed by the fixed points in spacetime? (Answer: ?)
;

(define (manhat-dist pnt-a pnt-b)
  (apply +
         (list (abs (- (first  pnt-a) (first  pnt-b)))
               (abs (- (second pnt-a) (second pnt-b)))
               (abs (- (third  pnt-a) (third  pnt-b)))
               (abs (- (fourth pnt-a) (fourth pnt-b))))))

(define (get-data filename)
  (let ((lines (file->lines filename))
        (spacetime (make-hash)))
    (for ((line lines))
         (define-values (x y z t) (apply values (map string->number (regexp-match* #rx"([-0-9]+)" line))))
         (hash-set! spacetime (list x y z t) #f))
    spacetime))

(define (get-linkages spacetime)
  (let ((linkages (make-hash)))
   (for ((coor-a (hash-keys spacetime)))
        (for ((coor-b (hash-keys spacetime)))
             (when (not (equal? coor-a coor-b))
               (when (<= (manhat-dist coor-a coor-b) 3)
                 (if (hash-has-key? linkages coor-a)
                     (when (not (member coor-b (hash-ref linkages coor-a)))
                       (hash-set! linkages coor-a (cons coor-b (hash-ref linkages coor-a))))
                     (hash-set! linkages coor-a (list coor-b)))
                 (if (hash-has-key? linkages coor-b)
                     (when (not (member coor-a (hash-ref linkages coor-b)))
                       (hash-set! linkages coor-b (cons coor-a (hash-ref linkages coor-b))))
                     (hash-set! linkages coor-b (list coor-a)))))))
   linkages))

(define (get-constellations spacetime linkages)
  (let ((constellations (make-hash))
        (constellation-index 0))

    (define (helper coor)
      (when (not (hash-has-key? constellations coor))
        (hash-set! constellations coor constellation-index)
        (for ((coor-link (hash-ref linkages coor))) (helper coor-link))))

    (for ((coor (hash-keys spacetime)))
         (if (not (hash-has-key? linkages coor))
             (begin
               (set! constellation-index (add1 constellation-index))
               (hash-set! constellations coor constellation-index))
             (when (not (hash-has-key? constellations coor))
               (set! constellation-index (add1 constellation-index))
               (helper coor))))

    constellation-index))

(define (part-one filename)
  (let* ((spacetime (get-data filename))
         (linkages (get-linkages spacetime)))
    (get-constellations spacetime linkages)))

(check-equal? (part-one "test1-data.txt") 2)
(check-equal? (part-one "test2-data.txt") 4)
(check-equal? (part-one "test3-data.txt") 3)
(check-equal? (part-one "test4-data.txt") 8)

(check-equal? (part-one "day25-data.txt") 388)

;
; --- Part Two ---
;
; A small glowing portal opens above the mug you prepared and just enough
;  hot chocolate streams in to fill it. You suspect the reindeer has never
;  encountered hot chocolate before, but seems to enjoy it anyway.
;  You hope it works.
;
; It's time to start worrying about that integer underflow in time itself
;  you set up a few days ago. You check the status of the device: "Insufficient
;  chronal energy for activation. Energy required: 50 stars."
;
; The reindeer bumps the device with its nose.
;
; "Energy required: 49 stars."
;
; You have enough stars to "Trigger the Underflow".
;                                    |
;                                    |
;                                    v
;
; You use all fifty stars to activate the underflow. As you begin to fall,
;  the little reindeer looks up at you; its nose begins to glow red.
;
; You go back in time so far that you wrap around and end up back in your own
;  time again! Oddly, history books contain some new details that you don't
;  recognize...
;
; Congratulations! You've finished every puzzle in Advent of Code 2018! I hope
;  you had as much fun solving them as I had making them for you. I'd love to
;  hear about your adventure; you can get in touch with me via contact info on
;  my website or through Twitter or Mastodon.
;
; If you'd like to see more things like this in the future, please consider
;  supporting Advent of Code and sharing it with others.
;
; To hear about future projects, you can follow me on Twitter or Mastodon.
;
; I've highlighted the easter eggs in each puzzle, just in case you missed any.
;  Hover your mouse over them, and the easter egg will appear.
;

