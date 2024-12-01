#lang racket

;; Advent of Code 2019 - Day 17
;; Functional Racket implementation with mutable hash tables

(require racket/string)
(require racket/file)
(require racket/vector)

;; Define the IntcodeState structure
(struct IntcodeState
  (memory pointer relative-base inputs outputs halted?)
  #:transparent)

;; Initialize the Intcode state with memory, pointer, relative-base, inputs, outputs, and halted flag
(define (init-intcode memory)
  (IntcodeState
   memory        ;; memory: mutable hash table mapping addresses to values
   0             ;; pointer: instruction pointer
   0             ;; relative-base
   '()           ;; inputs: list of input values
   '()           ;; outputs: list of output values
   #f))          ;; halted?: boolean flag indicating if the program has halted

;; Parse the Intcode program from a string into a mutable hash table memory
(define (parse-program input)
  (let* ([codes (map string->number (string-split (string-trim input) ","))]
         [memory (make-hash)]) ;; Create a mutable hash table
    ;; Populate the mutable hash table with the Intcode program
    (for ([i (in-naturals)]
          [code codes])
      (hash-set! memory i code))
    memory)) ;; Return the mutable hash table

;; Get a value from memory with default 0 if the address is not present
(define (get-memory memory addr)
  (hash-ref memory addr 0))

;; Set a value in memory based on parameter mode, modifying memory in place
(define (set-param memory param mode relative-base val)
  (cond
    [(= mode 0) ;; Position mode
     (hash-set! memory param val)]
    [(= mode 2) ;; Relative mode
     (hash-set! memory (+ relative-base param) val)]
    [else
     (error "Unknown parameter mode for writing:" mode)]))

;; Get a parameter value based on its mode
(define (get-param memory param mode relative-base)
  (cond
    [(= mode 0) ;; Position mode
     (get-memory memory param)]
    [(= mode 1) ;; Immediate mode
     param]
    [(= mode 2) ;; Relative mode
     (get-memory memory (+ relative-base param))]
    [else
     (error "Unknown parameter mode:" mode)]))

;; Run the Intcode program recursively
(define (run-intcode state)
  (let loop ([state state])
    (if (IntcodeState-halted? state)
        state
        (let* ([memory (IntcodeState-memory state)]
               [pointer (IntcodeState-pointer state)]
               [relative-base (IntcodeState-relative-base state)]
               [instr (get-memory memory pointer)]
               [opcode (modulo instr 100)]
               [m1 (quotient (modulo instr 1000) 100)]
               [m2 (quotient (modulo instr 10000) 1000)]
               [m3 (quotient (modulo instr 100000) 10000)]
               [param1 (get-memory memory (+ pointer 1))]
               [param2 (get-memory memory (+ pointer 2))]
               [param3 (get-memory memory (+ pointer 3))])
          (cond
            [(= opcode 1) ;; Add
             (let* ([val1 (get-param memory param1 m1 relative-base)]
                    [val2 (get-param memory param2 m2 relative-base)])
               (set-param memory param3 m3 relative-base (+ val1 val2))
               (loop (IntcodeState memory (+ pointer 4) relative-base
                                    (IntcodeState-inputs state)
                                    (IntcodeState-outputs state)
                                    #f)))]
            
            [(= opcode 2) ;; Multiply
             (let* ([val1 (get-param memory param1 m1 relative-base)]
                    [val2 (get-param memory param2 m2 relative-base)])
               (set-param memory param3 m3 relative-base (* val1 val2))
               (loop (IntcodeState memory (+ pointer 4) relative-base
                                    (IntcodeState-inputs state)
                                    (IntcodeState-outputs state)
                                    #f)))]
            
            [(= opcode 3) ;; Input
             (if (null? (IntcodeState-inputs state))
                 ;; If no input is available, halt waiting for input
                 state
                 ;; Otherwise, consume the first input and continue
                 (let* ([input (car (IntcodeState-inputs state))]
                        [remaining-inputs (cdr (IntcodeState-inputs state))])
                   (set-param memory param1 m1 relative-base input)
                   (loop (IntcodeState memory (+ pointer 2) relative-base
                                        remaining-inputs
                                        (IntcodeState-outputs state)
                                        #f))))]
            
            [(= opcode 4) ;; Output
             (let* ([val (get-param memory param1 m1 relative-base)]
                    [new-outputs (append (IntcodeState-outputs state) (list val))])
               (loop (IntcodeState memory (+ pointer 2) relative-base
                                    (IntcodeState-inputs state)
                                    new-outputs
                                    #f)))]
            
            [(= opcode 5) ;; Jump-if-true
             (let ([val (get-param memory param1 m1 relative-base)]
                   [target (get-param memory param2 m2 relative-base)])
               (if (not (= val 0))
                   (loop (IntcodeState memory target relative-base
                                        (IntcodeState-inputs state)
                                        (IntcodeState-outputs state)
                                        #f))
                   (loop (IntcodeState memory (+ pointer 3) relative-base
                                        (IntcodeState-inputs state)
                                        (IntcodeState-outputs state)
                                        #f))))]
            
            [(= opcode 6) ;; Jump-if-false
             (let ([val (get-param memory param1 m1 relative-base)]
                   [target (get-param memory param2 m2 relative-base)])
               (if (= val 0)
                   (loop (IntcodeState memory target relative-base
                                        (IntcodeState-inputs state)
                                        (IntcodeState-outputs state)
                                        #f))
                   (loop (IntcodeState memory (+ pointer 3) relative-base
                                        (IntcodeState-inputs state)
                                        (IntcodeState-outputs state)
                                        #f))))]
            
            [(= opcode 7) ;; Less than
             (let* ([val1 (get-param memory param1 m1 relative-base)]
                    [val2 (get-param memory param2 m2 relative-base)]
                    [result (if (< val1 val2) 1 0)])
               (set-param memory param3 m3 relative-base result)
               (loop (IntcodeState memory (+ pointer 4) relative-base
                                    (IntcodeState-inputs state)
                                    (IntcodeState-outputs state)
                                    #f)))]
            
            [(= opcode 8) ;; Equals
             (let* ([val1 (get-param memory param1 m1 relative-base)]
                    [val2 (get-param memory param2 m2 relative-base)]
                    [result (if (= val1 val2) 1 0)])
               (set-param memory param3 m3 relative-base result)
               (loop (IntcodeState memory (+ pointer 4) relative-base
                                    (IntcodeState-inputs state)
                                    (IntcodeState-outputs state)
                                    #f)))]
            
            [(= opcode 9) ;; Adjust relative base
             (let ([val (get-param memory param1 m1 relative-base)])
               (loop (IntcodeState memory (+ pointer 2) (+ relative-base val)
                                    (IntcodeState-inputs state)
                                    (IntcodeState-outputs state)
                                    #f)))]
            
            [(= opcode 99) ;; Halt
             (IntcodeState memory pointer relative-base
                           (IntcodeState-inputs state)
                           (IntcodeState-outputs state)
                           #t)]
            
            [else
             (error "Unknown opcode:" opcode)])))))

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

;; Define the main routine and movement functions as strings
;; [A,B,A,C,B,C,A,B,A,C R,10,L,8,R,10,R,4 L,6,L,6,R,10 L,6,R,12,R,12,R,10] <- from day17.go
(define main-routine "A,B,A,C,B,C,A,B,A,C")
(define function-a "R,10,L,8,R,10,R,4")
(define function-b "L,6,L,6,R,10")
(define function-c "L,6,R,12,R,12,R,10")

;; Convert a string to a list of ASCII codes, appending a newline character (10)
(define (string->ascii-list str)
  (append (map char->integer (string->list str)) (list 10)))

;; Main function to execute the steps
(define (main)
  ;; Read the Intcode program from 'day17-data.txt'
  (define input (file->string "day17-data.txt"))
  (define program (parse-program input))
  
  ;; Activate movement mode by setting address 0 to 2
  (hash-set! program 0 2)
  
;;   ;; Define the main routine and movement functions
;;   (define main-routine "A,B,A,C,B,C,A,B,A")
;;   (define function-a "R,10,L,8,R,10,R,4")
;;   (define function-b "L,6,L,6,R,10")
;;   (define function-c "L,6,R,12,R,12,R,10")
  
  ;; Encode the instructions into ASCII
  (define encoded-main (string->ascii-list main-routine))
  (define encoded-a (string->ascii-list function-a))
  (define encoded-b (string->ascii-list function-b))
  (define encoded-c (string->ascii-list function-c))
  
  ;; Optionally, define whether to display continuous video feed
  (define continuous-video-feed (string->ascii-list "n")) ;; 'n' for no, 'y' for yes
  
  ;; Combine all inputs: main routine, functions A, B, (C), and video feed
  (define combined-inputs
    (append encoded-main
            encoded-a
            encoded-b
            encoded-c
            continuous-video-feed))
  
  ;; Initialize the Intcode state with the program and inputs
  (define initial-state (init-intcode program))
  
  ;; Add inputs to the Intcode state
  (define state-with-inputs
    (IntcodeState
     (IntcodeState-memory initial-state)
     (IntcodeState-pointer initial-state)
     (IntcodeState-relative-base initial-state)
     combined-inputs
     (IntcodeState-outputs initial-state)
     (IntcodeState-halted? initial-state)))
  
  ;; Run the Intcode program with inputs
  (define final-state-part2 (run-intcode state-with-inputs))
  
  ;; Collect outputs
  (define outputs-part2 (IntcodeState-outputs final-state-part2))
  
  ;; The final output is the amount of dust collected
  (define dust-amount (last outputs-part2))
  
  ;; Print the dust amount
  (printf "Part 2: Dust collected = ~a\n" dust-amount)
  
  ;; Optionally, you can also print the final grid or other outputs if needed
  )

;; Execute the main function
(main) ; (Answer should be: 1045393)
