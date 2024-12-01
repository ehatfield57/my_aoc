#lang racket

;; intcode.rkt
;; Intcode Interpreter Module for Advent of Code 2019

(provide parse-program init-intcode run-intcode add-input get-outputs IntcodeState-memory)

(require racket/hash)

;; Define the IntcodeState structure
(struct IntcodeState
  (memory pointer relative-base inputs outputs halted?)
  #:transparent)

;; Parse the Intcode program from a string into a mutable hash table memory
(define (parse-program input)
  (let* ([codes (map string->number (string-split (string-trim input) ","))]
         [memory (make-hash)]) ;; Create a mutable hash table
    ;; Populate the mutable hash table with the Intcode program
    (for ([i (in-naturals)]
          [code codes])
      (hash-set! memory i code))
    memory)) ;; Return the mutable hash table

;; Initialize the Intcode state with memory, pointer, relative-base, inputs, outputs, and halted flag
(define (init-intcode memory)
  (IntcodeState
   memory        ;; memory: mutable hash table mapping addresses to values
   0             ;; pointer: instruction pointer
   0             ;; relative-base
   '()           ;; inputs: list of input values
   '()           ;; outputs: list of output values
   #f))          ;; halted?: boolean flag indicating if the program has halted

;; Add an input value to the Intcode state
(define (add-input state input)
  (IntcodeState
   (IntcodeState-memory state)
   (IntcodeState-pointer state)
   (IntcodeState-relative-base state)
   (append (IntcodeState-inputs state) (list input))
   (IntcodeState-outputs state)
   (IntcodeState-halted? state)))

;; Retrieve the outputs from the Intcode state
(define (get-outputs state)
  (IntcodeState-outputs state))

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

