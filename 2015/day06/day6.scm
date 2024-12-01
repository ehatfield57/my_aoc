
(require rackunit)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 6: Probably a Fire Hazard ---
;
; Because your neighbors keep defeating you in the holiday house decorating
;  contest year after year, you've decided to deploy one million lights in
;  a 1000x1000 grid.
; 
; Furthermore, because you've been especially nice this year, Santa has mailed
;  you instructions on how to display the ideal lighting configuration.
; 
; Lights in your grid are numbered from 0 to 999 in each direction; the lights
;  at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions
;  include whether to turn on, turn off, or toggle various inclusive ranges
;  given as coordinate pairs. Each coordinate pair represents opposite corners
;  of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore
;  refers to 9 lights in a 3x3 square. The lights all start turned off.
; 
; To defeat your neighbors this year, all you have to do is set up your lights
;  by doing the instructions Santa sent you in order.
; 
; For example:
; 
; * turn on 0,0 through 999,999 would turn on (or leave on) every light.
;
; * toggle 0,0 through 999,0 would toggle the first line of 1000 lights,
;    turning off the ones that were on, and turning on the ones that were off.
;
; * turn off 499,499 through 500,500 would turn off (or leave off) the middle
;    four lights.
;
; After following the instructions, how many lights are lit? (Answer: 400410)
;
; Three lines from data file:
; turn off 199,133 through 461,193
; toggle 537,781 through 687,941
; turn on 226,196 through 599,390
;

(define array-size 1000)
(define lights (for/vector ([i (in-range array-size)]) (make-vector array-size 0)))

(define get-data
  (lambda (str)
    (define normalize-info
      (lambda (lst)
        (cons
          (match (cadr lst)
                 ["turn on" 'on]
                 ["turn off" 'off]
                 ["toggle"   'toggle])
          (map string->number (cddr lst)))))
    (normalize-info (regexp-match
                      #rx"(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"
                      str))))

(check-equal? (get-data "turn off 199,133 through 461,193") '(off 199 133 461 193))
(check-equal? (get-data "toggle 537,781 through 687,941") '(toggle 537 781 687 941))
(check-equal? (get-data "turn on 226,196 through 599,390") '(on 226 196 599 390))


(define light-ref  (lambda (x y)     (vector-ref  (vector-ref lights y) x)))
(define light-set! (lambda (x y val) (vector-set! (vector-ref lights y) x val)))

(check-equal? (light-ref 0 0) 0)
(light-set! 0 0 1)
(check-equal? (light-ref 0 0) 1)
(light-set! 0 0 0) ; Be sure to set the value back to zero after testing
(check-equal? (light-ref 0 0) 0)

(define rect->points
  (lambda (a b x y)
    (and (< x a) (error "Boo"))
    (and (< y b) (error "Hoo"))
    (let ((lst '()))
     (for ([m (in-range a (add1 x))])
       (for ([n (in-range b (add1 y))])
         (set! lst (cons (list m n) lst))))
     lst)))

(check-equal? (rect->points 0 0 2 2) '((2 2) (2 1) (2 0) (1 2) (1 1) (1 0) (0 2) (0 1) (0 0)))


(define flip     (lambda (x) (if (= 1 x) 0 1)))
(define turn-on  (lambda (point) (light-set! (car point) (cadr point) 1)))
(define turn-off (lambda (point) (light-set! (car point) (cadr point) 0)))
(define toggle   (lambda (point) (light-set! (car point) (cadr point) (flip (light-ref (car point) (cadr point))))))

(check-equal? 1 (flip 0))
(check-equal? 0 (flip 1))
(turn-on '(0 0))
(check-equal? (light-ref 0 0) 1)
(turn-off '(0 0))
(check-equal? (light-ref 0 0) 0)

(define count-lights
  (lambda ()
    (let ((total 0))
     (for ([m (in-range 0 array-size)])
       (for ([n (in-range 0 array-size)])
         (set! total (+ total (light-ref m n)))))
     total)))

(define reset-lights
  (lambda ()
    (for ([m (in-range 0 array-size)])
      (for ([n (in-range 0 array-size)])
        (light-set! m n 0)))))

(check-equal? 0 (count-lights))
(turn-on '(2 2))
(check-equal? 1 (count-lights))
(toggle '(2 2))
(check-equal? 0 (count-lights))


(define execute
  (lambda (cmd a b x y)
    (for-each (cond
                ((equal? cmd 'on)     turn-on)
                ((equal? cmd 'off)    turn-off)
                ((equal? cmd 'toggle) toggle))
              (rect->points a b x y))))

(check-equal? 0 (count-lights))
(apply execute (get-data "turn on 499,499 through 500,500"))
(check-equal? 4 (count-lights))
(apply execute (get-data "turn off 499,499 through 500,500"))
(check-equal? 0 (count-lights))

(check-equal? 0 (count-lights))
(apply execute (get-data "turn on 0,0 through 999,999"))
(check-equal? 1000000 (count-lights))
(apply execute (get-data "turn off 0,0 through 999,999"))
(check-equal? 0 (count-lights))

(check-equal? 0 (count-lights))
(apply execute (get-data "toggle 0,0 through 999,0"))
(check-equal? 1000 (count-lights))
(apply execute (get-data "toggle 0,0 through 999,0"))
(check-equal? 0 (count-lights))


(define part-one
  (lambda (filename)
    (reset-lights)
    (with-input-from-file filename
                          (lambda ()
                            (for ([line (in-lines)])
                              (apply execute (get-data line)))))
    (displayln (count-lights))
    (count-lights)))

(check-equal? (part-one "day6-data.txt") 400410)
(displayln "Part one done")


; --- Part Two ---
;
; You just finish implementing your winning light pattern when you realize you
;  mistranslated Santa's message from Ancient Nordic Elvish.
; 
; The light grid you bought actually has individual brightness controls; each
;  light can have a brightness of zero or more. The lights all start at zero.
; 
; The phrase turn on actually means that you should increase the brightness of
;  those lights by 1.
; 
; The phrase turn off actually means that you should decrease the brightness of
;  those lights by 1, to a minimum of zero.
; 
; The phrase toggle actually means that you should increase the brightness of
;  those lights by 2.
; 
; What is the total brightness of all lights combined after following Santa's
;  instructions? (Answer: 15343601)
; 
; For example:
; 
; * turn on 0,0 through 0,0 would increase the total brightness by 1.
;
; * toggle 0,0 through 999,999 would increase the total brightness by 2000000.
;

(define part-two
  (lambda (filename)
    (reset-lights)

    (define turn-on
      (lambda (point)
        (let ((x (car point))
              (y (cadr point)))
          (light-set! x y (+ 1 (light-ref x y))))))

    (define turn-off
      (lambda (point)
        (let ((x (car point))
              (y (cadr point)))
          (light-set! x y (- (light-ref x y) (if (> (light-ref x y) 0) 1 0))))))

    (define toggle
      (lambda (point)
        (let ((x (car point))
              (y (cadr point)))
          (light-set! x y (+ 2 (light-ref x y))))))

    (with-input-from-file filename
                          (lambda ()
                            (for ([line (in-lines)])
                              (apply execute (get-data line)))))
    (displayln (count-lights))
    (count-lights)))

(check-equal? 15343601 (part-two "day6-data.txt"))
(displayln "Part two done")

