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

;; Main function to execute the steps
(define (main)
  ;; Read the Intcode program from 'day17-data.txt'
  (define input (file->string "day17-data.txt"))
  (define program (parse-program input))
  
  ;; Initialize the Intcode state with the program and no inputs
  (define initial-state (init-intcode program))
  
  ;; Run the Intcode program
  (define final-state (run-intcode initial-state))
  
  ;; Collect outputs
  (define outputs (IntcodeState-outputs final-state))
  
  ;; Convert outputs to grid
  (define grid (ascii-to-grid outputs))
  
  ;; Print the grid
  (print-grid grid)
  
  ;; Calculate and print the sum of alignment parameters (Part 1)
  (define alignment-sum (find-intersections grid))
  (printf "Part 1: Sum of alignment parameters = ~a\n" alignment-sum)
  
  ;; Placeholder for Part 2
  ;; Implementing Part 2 involves providing movement routines to the Intcode program.
  ;; This typically requires analyzing the scaffold path, identifying patterns,
  ;; and sending the appropriate ASCII commands to the Intcode program.
  ;; Due to its complexity, Part 2 is not implemented here.
  )

;; Execute the main function
(main)
