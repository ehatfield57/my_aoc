#lang racket

(require racket/string
         racket/math)

;; Define a structure for Reactions
(struct reaction (output-qty inputs) #:transparent)

;; Function to parse a single reaction line
(define (parse-reaction-line line)
  ;; Split the line into inputs and output using " => "
  (define parts (string-split line " => "))
  (unless (= (length parts) 2)
    (error "Invalid reaction line format:" line))
  (define inputs-str (first parts))
  (define output-str (second parts))
  
  ;; Parse inputs: split by ", " and parse each input component
  (define inputs
    (map (lambda (s)
           (define tokens (string-split s " "))
           (cons (string->symbol (second tokens)) ; Chemical name as symbol
                 (string->number (first tokens)))) ; Quantity as number
         (string-split inputs-str ", ")))
  
  ;; Parse output: split by space to get quantity and chemical
  (define output-tokens (string-split output-str " "))
  (define output-qty (string->number (first output-tokens)))
  (define output-chem (string->symbol (second output-tokens)))
  
  ;; Create the reaction and return as (output-chem . reaction)
  (cons output-chem (reaction output-qty inputs)))

;; Function to parse all reactions from the input string
(define (parse-reactions input)
  (define lines (string-split input "\n"))
  (define reactions-list (map parse-reaction-line lines))
  ;; Use for/hash to create a hash from (key . value) pairs
  (for/hash ([pair reactions-list])
    (car pair) ; Key: output chemical
    (cdr pair))) ; Value: reaction struct

;; Function to compute ORE required for a given amount of FUEL
(define (compute-ore reactions fuel-amount)
  (define needs (make-hash))   ; Chemicals needed
  (define surplus (make-hash)) ; Surplus chemicals
  (define total-ore 0)         ; Total ORE required
  
  ;; Initialize needs with the required amount of FUEL
  (hash-set! needs 'FUEL fuel-amount)
  
  ;; Define an iterative loop to process needs
  (define (loop)
    ;; Get the list of chemicals currently needed (excluding ORE)
    (define keys (filter (lambda (k) (not (eq? k 'ORE)))
                         (hash-keys needs)))
    (unless (empty? keys)
      ;; Process one chemical at a time (FIFO approach)
      (define chem (first keys))
      (define amt (hash-ref needs chem))
      ;; Remove the chemical from needs
      (hash-remove! needs chem)
      
      ;; Check and use surplus
      (define available (hash-ref surplus chem 0))
      (define used (min available amt))
      (when (> used 0)
        (hash-set! surplus chem (- available used))
        (set! amt (- amt used)))
      
      ;; If still need to produce more
      (when (> amt 0)
        (define reaction (hash-ref reactions chem))
        (define output_qty (reaction-output-qty reaction))
        (define times (ceiling (/ amt output_qty)))
        (define produced (* times output_qty))
        (define extra (- produced amt))
        ;; Update surplus with extra
        (when (> extra 0)
          (hash-set! surplus chem (+ (hash-ref surplus chem 0) extra)))
        ;; Add inputs to needs
        (for-each (lambda (input)
                    (define input-chem (car input))
                    (define input-qty (* (cdr input) times))
                    (hash-update! needs input-chem
                                 (lambda (old)
                                   (+ old input-qty))
                                 0))
                  (reaction-inputs reaction)))
      
      ;; Continue looping
      (loop)))
  
  ;; Start processing needs
  (loop)
  
  ;; Return the total ORE required
  total-ore)

;; Function to solve Part 1
(define (solve-part1 reactions)
  (compute-ore reactions 1))

;; Main Execution

;; Read input from 'day14-data.txt'
(define input (file->string "day14-data.txt"))

;; Parse reactions
(define reactions (parse-reactions input))

;; Solve Part 1
(define part1 (solve-part1 reactions))
(displayln (string-append "Part 1: ORE required for 1 FUEL: " (number->string part1)))
