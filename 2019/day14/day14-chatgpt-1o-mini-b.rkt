#lang racket

(require racket/string
         racket/math)

;; Define a structure for Reactions
(struct reaction ([output-qty :exact-integer?]
                [inputs :list-of (cons symbol exact-integer?)]))

;; Parse a single reaction line
(define (parse-reaction-line line)
  (define-values (inputs-str output-str) (regexp-match #px"(.+) => (.+)" line))
  (define inputs
    (map (λ (s)
           (define parts (string-split s " "))
           (cons (string->symbol (second parts))
                 (exact-string->exact-integer (first parts))))
         (string-split inputs-str ", ")))
  (define-values (output-qty output-chem)
    (let ([parts (string-split output-str " ")])
      (values (exact-string->exact-integer (first parts))
              (string->symbol (second parts)))))
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
  (define total-ore 0)
  
  (define (produce chem amount)
    (if (eq? chem ORE)
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
            (define output-qty (reaction-output-qty reaction))
            (define times (ceiling (/ (exact->inexact amount) 
                                      (exact->inexact output-qty))))
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
  (produce ORE 0) ;; Initialize ORE
  (produce 'FUEL fuel-amount)
  
  total-ore)

;; Function to solve Part 1
(define (solve-part1 reactions)
  (compute-ore reactions 1))

;; Function to solve Part 2
(define (solve-part2 reactions total-ore-available)
  (define lower 1)
  (define upper 1)
  
  ;; First, find an upper bound
  (let loop ([current-upper upper])
    (define required (compute-ore reactions current-upper))
    (if (> required total-ore-available)
        current-upper
        (loop (* 2 current-upper))))
  
  ;; Binary search between lower and upper
  (let ([upper-bound (loop upper)])
    (define (binary-search low high)
      (if (> low high)
          (- low 1)
          (let ([mid (quotient (+ low high) 2)]
                [required (compute-ore reactions (quotient (+ low high) 2))])
            (cond
              [(> required total-ore-available) (binary-search low (- mid 1))]
              [else (binary-search mid high)]))))
    (binary-search lower upper-bound)))

;; Example Usage:

;; Example Input
(define example-input 
  "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

;; Parse reactions
(define reactions (parse-reactions example-input))

;; Solve Part 1
(define part1 (solve-part1 reactions))
(displayln (string-append "Part 1: ORE required for 1 FUEL: " (number->string part1)))

;; Example for Part 2 with a large ORE amount
(define total-ore-available 1000000000000)
(define part2 (solve-part2 reactions total-ore-available))
(displayln (string-append "Part 2: Maximum FUEL for " 
                           (number->string total-ore-available) 
                           " ORE: " 
                           (number->string part2)))

;; To handle actual problem input, you can read from a file:
;; (define input (file->string "path_to_input_file.txt"))
;; (define reactions (parse-reactions input))
;; (define part1 (solve-part1 reactions))
;; (displayln (string-append "Part 1: " (number->string part1)))
;; (define part2 (solve-part2 reactions 1000000000000))
;; (displayln (string-append "Part 2: " (number->string part2)))
