
(require rackunit)

;
; Advent of Code - 2016
;
; Part One:
;
; --- Day 8: Two-Factor Authentication ---
;
; You come across a door implementing what you can only assume is an implementation
;  of two-factor authentication after a long game of requirements telephone.
;
; To get past the door, you first swipe a keycard (no problem; there was one on a nearby
;  desk). Then, it displays a code on a little screen, and you type that code on a keypad.
;  Then, presumably, the door unlocks.
;
; Unfortunately, the screen has been smashed. After a few minutes, you've taken everything
;  apart and figured out how it works. Now you just have to work out what the screen would
;  have displayed.
;
; The magnetic strip on the card you swiped encodes a series of instructions for the screen;
;  these instructions are your puzzle input. The screen is 50 pixels wide and 6 pixels tall,
;  all of which start off, and is capable of three somewhat peculiar operations:
;
; * rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which
;    is A wide and B tall.
; * rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels.
;    Pixels that would fall off the right end appear at the left end of the row.
; * rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down
;    by B pixels. Pixels that would fall off the bottom appear at the top of the column.
;
; For example, here is a simple sequence on a smaller screen:
;
; * rect 3x2 creates a small rectangle in the top-left corner:
;
; ###....
; ###....
; .......
;
; * rotate column x=1 by 1 rotates the second column down by one pixel:
;
; #.#....
; ###....
; .#.....
;
; * rotate row y=0 by 4 rotates the top row right by four pixels:
;
; ....#.#
; ###....
; .#.....
;
; * rotate column x=1 by 1 again rotates the second column down by one pixel, causing the
;    bottom pixel to wrap back to the top:
;
; .#..#.#
; #.#....
; .#.....
;
; * As you can see, this display technology is extremely powerful, and will soon dominate the
;    tiny-code-displaying-screen market. That's what the advertisement on the back of the display
;    tries to convince you, anyway.
;
; There seems to be an intermediate check of the voltage used by the display: after you swipe
;  your card, if the screen did work, how many pixels should be lit? (Answer: 123)
;

; row = y and col = x

(define on  #\#)
(define off #\.)

(define (screen width height) ; test: (7 3), Real: (50 6)
  (let* ((pixels (make-vector (* width height) off))
         (rowcol->i (lambda (row col) (+ (* row width) col)))

         (get-pixel (lambda (row col)   (vector-ref  pixels (rowcol->i row col))))
         (put-pixel (lambda (row col p) (vector-set! pixels (rowcol->i row col) p)))
         (pixel-on  (lambda (row col)   (put-pixel row col on)))
         (pixel-off (lambda (row col)   (put-pixel row col off)))

         (show (lambda (cmd-str)
                 (printf "~n~a:~n" cmd-str)
                 (let ((cnt 0))
                  (for ((row height))
                       (for ((col width))
                            (printf "~a" (get-pixel row col))
                            (when (char=? on (get-pixel row col)) (set! cnt (add1 cnt))))
                       (printf "~n"))
                  (printf "Lights lit: ~a~n" cnt))))

         (shift-right (lambda (row)
                        (let ((temp (get-pixel row (sub1 width))))
                         (do ((col (sub1 width) (sub1 col)))
                             ((= col 0) #t)
                             (put-pixel row col (get-pixel row (sub1 col))))
                         (put-pixel row 0 temp))))

         (shift-down (lambda (col)
                       (let ((temp (get-pixel (sub1 height) col)))
                        (do ((row (sub1 height) (sub1 row)))
                            ((= row 0) #t)
                            (put-pixel row col (get-pixel (sub1 row) col)))
                        (put-pixel 0 col temp)))))

    (lambda (cmd-str)
      (cond
        ((regexp-match #rx"^point " cmd-str)
         (define-values (_ row-str col-str) (apply values (car (regexp-match* #rx"^point ([0-9]+)x([0-9]+)" cmd-str #:match-select values))))
         (pixel-on (string->number row-str) (string->number col-str)))

        ((regexp-match #rx"^rect " cmd-str)
         (define-values (_ col-str row-str) (apply values (car (regexp-match* #rx"^rect ([0-9]+)x([0-9]+)" cmd-str #:match-select values))))
         (for ((row (string->number row-str)))
              (for ((col (string->number col-str)))
                   (pixel-on row col))))

        ((regexp-match #rx"^rotate row " cmd-str)
         (define-values (_ row-str cnt-str) (apply values (car (regexp-match* #rx"^rotate row y=([0-9]+) by ([0-9]+)" cmd-str #:match-select values))))
         (for ((cnt (string->number cnt-str)))
              (shift-right (string->number row-str))))

        ((regexp-match #rx"^rotate column " cmd-str)
         (define-values (_ col-str cnt-str) (apply values (car (regexp-match* #rx"^rotate column x=([0-9]+) by ([0-9]+)" cmd-str #:match-select values))))
         (for ((cnt (string->number cnt-str)))
              (shift-down (string->number col-str))))

        (else (error "Unknown command string in screen: ~a~n" cmd-str)))
      (show cmd-str))))

(define (part-one screen data)
  (for ((cmd data))
       (screen cmd)))

(define test-data '("rect 3x2" "rotate column x=1 by 1" "rotate row y=0 by 4" "rotate column x=1 by 1"))
(define test (screen  7 3))
(part-one test test-data)

(define real (screen 50 6))
(part-one real (file->lines "day08-data.txt"))

; .##..####.###..#..#.###..####.###....##.###...###.
; #..#.#....#..#.#..#.#..#....#.#..#....#.#..#.#....
; #..#.###..###..#..#.#..#...#..###.....#.#..#.#....
; ####.#....#..#.#..#.###...#...#..#....#.###...##..
; #..#.#....#..#.#..#.#....#....#..#.#..#.#.......#.
; #..#.#....###...##..#....####.###...##..#....###..
;
; Lights lit: 123

;
; --- Part Two ---
;
; You notice that the screen is only capable of displaying capital letters; in the
;  font it uses, each letter is 5 pixels wide and 6 tall.
;
; After you swipe your card, what code is the screen trying to display? (Answer: AFBUPZBJPS)
;


