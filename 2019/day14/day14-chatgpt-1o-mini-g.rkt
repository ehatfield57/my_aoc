#lang racket

(require racket/string
         racket/math)

;; Define a structure for Reactions
(struct reaction (output-qty inputs) #:transparent)

;; Parse a single reaction line
(define (parse-reaction-line line)
  ;; Match the line with regex "(.+) => (.+)"
  (define match (regexp-match #px"(.+) => (.+)" line))
  (unless match
    (error "Invalid reaction line format" line))
  (define inputs-str (list-ref match 1))
  (define output-str (list-ref match 2))
  
  ;; Parse inputs
  (define inputs
    (map (lambda (s)
           (define parts (string-split s " "))
           (cons (string->symbol (list-ref parts 1))
                 (string->number (list-ref parts 0))))
         (string-split inputs-str ", ")))
  
  ;; Parse output
  (define output-parts (string-split output-str " "))
  (define output-qty (string->number (list-ref output-parts 0)))
  (define output-chem (string->symbol (list-ref output-parts 1)))
  
  ;; Return (output-chem . (reaction output-qty inputs))
  (cons output-chem (reaction output-qty inputs)))

;; Parse all reactions from input string
(define (parse-reactions input)
  (define reaction-list
    (map parse-reaction-line (string-split input "\n")))
  (hash reaction-list))

;; Function to compute ORE required for a given amount of FUEL
(define (compute-ore reactions fuel-amount)
  (let ([surplus (make-hash)]
        [total-ore 0])
    
    ;; Recursive function to produce a chemical
    (define (produce chem amount)
      (if (eq? chem 'ORE)
          (set! total-ore (+ total-ore amount))
          (begin
            ;; Check surplus
            (define available (hash-ref surplus chem 0))
            (when (> available 0)
              (define used (min available amount))
              (hash-set! surplus chem (- available used))
              (set! amount (- amount used)))
            (when (> amount 0)
              (define reaction (hash-ref reactions chem))
              (define output_qty (reaction-output-qty reaction))
              (define times (ceiling (/ amount output_qty)))
              (define produced (* times output_qty))
              (define extra (- produced amount))
              (when (> extra 0)
                (hash-set! surplus chem (+ (hash-ref surplus chem 0) extra)))
              ;; Produce inputs
              (for-each (lambda (input)
                          (produce (car input) (* (cdr input) times)))
                        (reaction-inputs reaction))))))
    
    ;; Start production
    (produce 'FUEL fuel-amount)
    
    ;; Return total ORE required
    total-ore))

;; Function to solve Part 1
(define (solve-part1 reactions)
  (compute-ore reactions 1))

;; Function to solve Part 2
(define (solve-part2 reactions total-ore-available)
  (define lower 1)
  (define upper 1)
  
  ;; Recursive function to find upper bound by doubling until required ORE exceeds available
  (define (find-upper current-upper)
    (define required (compute-ore reactions current-upper))
    (if (> required total-ore-available)
        current-upper
        (find-upper (* 2 current-upper))))
  
  (define upper-bound (find-upper upper))
  
  ;; Binary search between lower and upper to find the maximum FUEL producible
  (define (binary-search low high)
    (if (> low high)
        (- low 1)
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
