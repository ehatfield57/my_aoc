#lang racket

(require racket/string
         racket/math)

;; Define a structure for Reactions without type annotations
(struct reaction (output-qty inputs) #:transparent)

;; Parse a single reaction line
(define (parse-reaction-line line)
  (define match (regexp-match #px"(.+) => (.+)" line))
  (unless match
    (error "Invalid reaction line format" line))
  (define inputs-str (second match))
  (define output-str (third match))
  
  ;; Parse inputs
  (define inputs
    (map (λ (s)
           (define parts (string-split s " "))
           (cons (string->symbol (second parts))
                 (string->number (first parts))))
         (string-split inputs-str ", ")))
  
  ;; Parse output
  (define parts (string-split output-str " "))
  (define output-qty (string->number (first parts)))
  (define output-chem (string->symbol (second parts)))
  
  ;; Return a key-value pair for the hash map
  (cons output-chem (reaction output-qty inputs)))

;; Parse all reactions from input string
(define (parse-reactions input)
  (define reaction-list
    (map parse-reaction-line (string-split input "\n")))
  (hash reaction-list))

;; Function to compute ORE required for a given amount of FUEL
(define (compute-ore reactions fuel-amount)
  (define needs (make-hash))
  (hash-set! needs 'FUEL fuel-amount)
  (define surplus (make-hash))
  
  (define ORE 'ORE)
  (define total-ore (make-box 0))
  
  (define (produce chem amount)
    (if (eq? chem ORE)
        (set-box! total-ore (+ (unbox total-ore) amount))
        (let ([available (hash-ref surplus chem 0)])
          (when (> available 0)
            (define used (min available amount))
            (hash-set! surplus chem (- available used))
            (set! amount (- amount used)))
          (when (> amount 0)
            (define reaction (hash-ref reactions chem))
            (define output-qty (reaction-output-qty reaction))
            (define times (ceiling (/ amount output-qty)))
            ;; Add surplus
            (define produced (* times output-qty))
            (define extra (- produced amount))
            (when (> extra 0)
              (hash-set! surplus chem (+ (hash-ref surplus chem 0) extra)))
            ;; Produce inputs
            (for-each (λ (input)
                        (produce (car input) (* (cdr input) times)))
                      (reaction-inputs reaction))))))
  
  ;; Start production
  (produce 'FUEL fuel-amount)
  
  (unbox total-ore))

;; Function to solve Part 1
(define (solve-part1 reactions)
  (compute-ore reactions 1))

;; Function to solve Part 2
(define (solve-part2 reactions total-ore-available)
  (define lower 1)
  (define upper 1)
  
  ;; First, find an upper bound by exponentially increasing until ORE required exceeds available
  (define (find-upper current-upper)
    (define required (compute-ore reactions current-upper))
    (if (> required total-ore-available)
        current-upper
        (find-upper (* 2 current-upper))))
  
  (define upper-bound (find-upper upper))
  
  ;; Binary search between lower and upper
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
