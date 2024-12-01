#lang racket

(require racket/string
         racket/math)

;; Define a structure for Reactions
(struct reaction (output-qty inputs) #:transparent)

;; Parse a single reaction line
(define (parse-reaction-line line)
  ;; Split the line into inputs and output using " => "
  (define parts (string-split line " => "))
  (unless (= (length parts) 2)
    (error "Invalid reaction line format" line))
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

;; Parse all reactions from input string
(define (parse-reactions input)
  (define lines (string-split input "\n"))
  (define reactions-list (map parse-reaction-line lines))
  (hash-from-list reactions-list))

;; Function to compute ORE required for a given amount of FUEL
(define (compute-ore reactions fuel-amount)
  (define needs (make-hash)) ; Chemicals needed
  (define surplus (make-hash)) ; Surplus chemicals
  (define total-ore 0) ; Total ORE required
  
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
      
      (if (eq? chem 'ORE)
          ;; If the chemical is ORE, add the required amount directly
          (set! total-ore (+ total-ore amt))
          ;; Else, process the chemical via its reaction
          (let* ([available (hash-ref surplus chem 0)] ; Available surplus
                 [used (min available amt)] ; Surplus to use
                 [remaining-amt (- amt used)] ; Remaining amount needed after using surplus
                 ;; Update surplus after using
                 [_ (when (> used 0)
                      (hash-set! surplus chem (- available used)))]
                 ;; If still need to produce more
                 [reaction (hash-ref reactions chem)]
                 [output_qty (reaction-output-qty reaction)]
                 [times (ceiling (/ remaining-amt output_qty))] ; Number of reaction runs
                 [produced (* times output_qty)] ; Total produced from reactions
                 [extra (- produced remaining-amt)] ; Surplus produced
                 ;; Update surplus with extra
                 [_ (when (> extra 0)
                      (hash-set! surplus chem (+ (hash-ref surplus chem 0) extra)))]
                 [inputs (reaction-inputs reaction)]) ; Inputs required for the reaction
            ;; Add each input to needs
            (for-each (lambda (input)
                        (let ([input-chem (car input)]
                              [input-qty (* (cdr input) times)])
                          (hash-update! needs input-chem (lambda (old)
                                                           (+ old input-qty))
                                       0)))
                      inputs)))
      ;; Continue looping
      (loop)))
  
  ;; Start processing needs
  (loop)
  
  ;; Return the total ORE required
  total-ore)

;; Function to solve Part 1
(define (solve-part1 reactions)
  (compute-ore reactions 1))

;; Function to solve Part 2
(define (solve-part2 reactions total-ore-available)
  (define lower 1)
  (define upper 1)
  
  ;; Recursive function to find an upper bound by doubling until required ORE exceeds available
  (define (find-upper current-upper)
    (define required (compute-ore reactions current-upper))
    (if (> required total-ore-available)
        current-upper
        (find-upper (* 2 current-upper))))
  
  (define upper-bound (find-upper upper))
  
  ;; Binary search between lower and upper to find the maximum FUEL producible
  (define (binary-search low high)
    (if (> low high)
        (- low 1) ; Maximum FUEL that can be produced
        (let* ([mid (quotient (+ low high) 2)]
               [required (compute-ore reactions mid)])
          (cond
            [(> required total-ore-available) (binary-search low (- mid 1))]
            [else (binary-search mid high)]))))
  
  (binary-search lower upper-bound))

;; Main Execution

;; Read input from 'day14-data.txt'
(define input (file->string "day14-data.txt"))

;; Parse reactions
(define reactions (parse-reactions input))

;; Solve Part 1
(define part1 (solve-part1 reactions))
(displayln (string-append "Part 1: ORE required for 1 FUEL: " (number->string part1)))

;; Solve Part 2 with a large ORE amount
(define total-ore-available 1000000000000)
(define part2 (solve-part2 reactions total-ore-available))
(displayln (string-append "Part 2: Maximum FUEL for " 
                           (number->string total-ore-available) 
                           " ORE: " 
                           (number->string part2)))
