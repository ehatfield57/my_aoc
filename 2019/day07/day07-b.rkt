#lang racket

(define (parse-intcode input)
  (map string->number (string-split input ",")))

(define (get-param mode param intcode)
  (if (= mode 0)
      (list-ref intcode param)
      param))

(define (run-intcode intcode inputs)
  (define (step pc input outputs)
    (let* ([instruction (list-ref intcode pc)]
           [op (remainder instruction 100)] ; Modulus operation using remainder
           [mode1 (remainder (quotient instruction 100) 10)] ; Mode for parameter 1
           [mode2 (remainder (quotient instruction 1000) 10)] ; Mode for parameter 2
           [param1 (if (< (+ pc 1) (length intcode)) (list-ref intcode (+ pc 1)) 0)]
           [param2 (if (< (+ pc 2) (length intcode)) (list-ref intcode (+ pc 2)) 0)]
           [param3 (if (< (+ pc 3) (length intcode)) (list-ref intcode (+ pc 3)) 0)]
           [value1 (get-param mode1 param1 intcode)]
           [value2 (get-param mode2 param2 intcode)])
      (cond
        [(= op 1) (set! intcode (list-set intcode param3 (+ value1 value2)))
                 (step (+ pc 4) input outputs)]
        [(= op 2) (set! intcode (list-set intcode param3 (* value1 value2)))
                 (step (+ pc 4) input outputs)]
        [(= op 3) (set! intcode (list-set intcode param1 (car input)))
                 (step (+ pc 2) (cdr input) outputs)]
        [(= op 4) (step (+ pc 2) input (append outputs (list value1)))]
        [(= op 5) (step (if (not (= value1 0)) value2 (+ pc 3)) input outputs)]
        [(= op 6) (step (if (= value1 0) value2 (+ pc 3)) input outputs)]
        [(= op 7) (set! intcode (list-set intcode param3 (if (< value1 value2) 1 0)))
                 (step (+ pc 4) input outputs)]
        [(= op 8) (set! intcode (list-set intcode param3 (if (= value1 value2) 1 0)))
                 (step (+ pc 4) input outputs)]
        [(= op 99) outputs])))

  (step 0 inputs '()))

(define (permutations lst)
  (if (empty? lst)
      '(())
      (apply append
             (map (lambda (elem)
                    (map (lambda (perm) (cons elem perm))
                         (permutations (remove elem lst))))
                  lst))))

(define (amplifier-sequence intcode phases)
  (foldl (lambda (phase input)
           (run-intcode (vector->list intcode) (list phase input)))
         0
         phases))

(define (find-best-phase-setting intcode)
  (apply max
         (map (lambda (phases) (amplifier-sequence intcode phases))
              (permutations '(0 1 2 3 4)))))

;; Test with your input string, for example:
(define input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(define intcode (parse-intcode input))
(find-best-phase-setting intcode)
