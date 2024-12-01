#lang racket

;; day17.rkt
;; Main Program for Advent of Code 2019, Day 17

(require "intcode.rkt") ;; Ensure intcode.rkt is in the same directory

(require racket/string)
(require racket/file)
(require racket/vector)

;; Helper function: Convert a string to a list of ASCII codes, appending a newline character (10)
(define (string->ascii-list str)
  (append (map char->integer (string->list str)) (list 10)))

;; Convert ASCII outputs to a 2D grid (vector of vectors)
(define (ascii-to-grid outputs)
  (define chars (map integer->char outputs))
  (define combined-str (list->string chars))
  (define lines (filter (lambda (line) (not (string=? line "")))
                        (string-split combined-str "\n")))
  ;; Convert each line into a vector of characters
  (define vector-lines
    (map (lambda (line) (list->vector (string->list line))) lines))
  ;; Convert the list of vector lines into a vector of vectors
  (list->vector vector-lines))

;; Print the grid to the console
(define (print-grid grid)
  (for ([row (in-vector grid)])
    (for ([char (in-vector row)])
      (display (string char)))
    (newline)))

;; Check if a given cell is a scaffold ('#')
(define (is-scaffold? grid r c)
  (and (>= r 0) (< r (vector-length grid))
       (>= c 0) (< c (vector-length (vector-ref grid 0)))
       (char=? (vector-ref (vector-ref grid r) c) #\#)))

;; Find intersections and calculate the sum of alignment parameters
(define (find-intersections grid)
  (define height (vector-length grid))
  (define width (if (> height 0) (vector-length (vector-ref grid 0)) 0))
  (define intersections
    (for*/list ([r (in-range 1 (- height 1))]
               [c (in-range 1 (- width 1))]
               #:when (and (is-scaffold? grid r c)
                          (is-scaffold? grid (- r 1) c)    ;; Up
                          (is-scaffold? grid (+ r 1) c)    ;; Down
                          (is-scaffold? grid r (- c 1))    ;; Left
                          (is-scaffold? grid r (+ c 1))))  ;; Right
      (* r c)))
  (apply + intersections))

;; Function to prepare movement instructions (Part 2)
;; For demonstration purposes, these functions are hard-coded.
;; In a complete solution, you'd analyze the scaffold map to determine these.
(define (prepare-movement-instructions)
  ;; Define the main routine and movement functions as strings
  ;; These should be determined based on the scaffold map analysis
  (define main-routine "A,B,A,C,B,C,A,B,A")
  (define function-a "R,10,L,8,R,10,R,4")
  (define function-b "L,6,L,6,R,10")
  (define function-c "L,6,R,12,R,12,R,10")
  ;; ;; Example placeholders:
  ;; (define main-routine "A,B,A,B,A,B")
  ;; (define function-a "R,8,L,10,R,8")
  ;; (define function-b "L,6,R,8,R,8")
  ;; (define function-c "R,4,R,4,R,8") ;; Define if needed

  ;; Encode the instructions into ASCII
  (define encoded-main (string->ascii-list main-routine))
  (define encoded-a (string->ascii-list function-a))
  (define encoded-b (string->ascii-list function-b))
  (define encoded-c (string->ascii-list function-c))

  ;; Define whether to display continuous video feed ('n' or 'y'), followed by newline
  (define continuous-video-feed (string->ascii-list "n"))

  ;; Combine all inputs: main routine, functions A, B, (C), and video feed
  (append encoded-main
          encoded-a
          encoded-b
          encoded-c
          continuous-video-feed))

;; Main function to execute the steps
(define (main)
  ;; Read the Intcode program from 'day17-data.txt'
  (define input (file->string "day17-data.txt"))
  (define program (parse-program input))

  ;; Initialize the Intcode state with the program and no inputs
  (define initial-state (init-intcode program))

  ;; Run the Intcode program (Part 1)
  (define final-state-part1 (run-intcode initial-state))

  ;; Collect outputs
  (define outputs-part1 (get-outputs final-state-part1))

  ;; Convert outputs to grid
  (define grid (ascii-to-grid outputs-part1))

  ;; Print the grid
  (printf "Scaffold Map:\n")
  (print-grid grid)

  ;; Calculate and print the sum of alignment parameters (Part 1)
  (define alignment-sum (find-intersections grid))
  (printf "Part 1: Sum of alignment parameters = ~a\n" alignment-sum)


  ;; --- Part 2 ---

  ;; Prepare movement instructions
  ;; In a complete solution, these should be dynamically determined
  ;; based on the scaffold map. Here, they are hard-coded for demonstration.
  (define movement-instructions (prepare-movement-instructions))

  (hash-set! (IntcodeState-memory final-state-part1) 0 2)

  ;; Add movement instructions as inputs to the Intcode state
  (define state-with-inputs (add-input final-state-part1 movement-instructions))
  (printf "Hi Edward A, state-with-inputs:~a~n" state-with-inputs)

  ;; Run the Intcode program with movement instructions (Part 2)
  (define final-state-part2 (run-intcode state-with-inputs))
  (printf "Hi Edward B, final-state-part2:~a~n" final-state-part2)

  ;; Collect outputs
  (define outputs-part2 (get-outputs final-state-part2))
  (printf "Hi Edward C, outputs-part2:~a~n" outputs-part2)

  ;; The final output is the amount of dust collected (last output)
  (define dust-amount
    (if (null? outputs-part2)
        "No output received."
        (last outputs-part2)))

  ;; Print the dust amount
  (printf "Part 2: Dust collected = ~a\n" dust-amount))

;; Execute the main function
(main)
