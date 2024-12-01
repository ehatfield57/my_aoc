#lang racket
(require racket/list)

(define (read-program)
  (let* ([line (read-line)]
         [string-list (regexp-split #rx"," line)]
         [int-list (map string->number string-list)]
         [memory (list->vector int-list)])
    memory))

(define (execute-program memory)
  (define (execute ip outputs)
    (let ([instruction (vector-ref memory ip)])
      (let-values ([(opcode modes) (parse-instruction instruction)])
        (cond
          [(= opcode 99)
           (reverse outputs)]
          [(or (= opcode 1) (= opcode 2))
           (let* ([param1 (vector-ref memory (+ ip 1))]
                  [param2 (vector-ref memory (+ ip 2))]
                  [param3 (vector-ref memory (+ ip 3))]
                  [mode1 (list-ref modes 0)]
                  [mode2 (list-ref modes 1)]
                  [val1 (get-param-value memory param1 mode1)]
                  [val2 (get-param-value memory param2 mode2)]
                  [result (if (= opcode 1) (+ val1 val2) (* val1 val2))])
             (vector-set! memory param3 result)
             (execute (+ ip 4) outputs))]
          [(= opcode 3)
           (let* ([param (vector-ref memory (+ ip 1))]
                  [input-value 5]) ; Input is '5' for Part Two
             (vector-set! memory param input-value)
             (execute (+ ip 2) outputs))]
          [(= opcode 4)
           (let* ([param (vector-ref memory (+ ip 1))]
                  [mode (list-ref modes 0)]
                  [output-value (get-param-value memory param mode)])
             (execute (+ ip 2) (append outputs (list output-value))))]
          [(= opcode 5) ; Jump-if-true
           (let* ([param1 (vector-ref memory (+ ip 1))]
                  [param2 (vector-ref memory (+ ip 2))]
                  [mode1 (list-ref modes 0)]
                  [mode2 (list-ref modes 1)]
                  [val1 (get-param-value memory param1 mode1)]
                  [val2 (get-param-value memory param2 mode2)])
             (if (not (= val1 0))
                 (execute val2 outputs)
                 (execute (+ ip 3) outputs)))]
          [(= opcode 6) ; Jump-if-false
           (let* ([param1 (vector-ref memory (+ ip 1))]
                  [param2 (vector-ref memory (+ ip 2))]
                  [mode1 (list-ref modes 0)]
                  [mode2 (list-ref modes 1)]
                  [val1 (get-param-value memory param1 mode1)]
                  [val2 (get-param-value memory param2 mode2)])
             (if (= val1 0)
                 (execute val2 outputs)
                 (execute (+ ip 3) outputs)))]
          [(= opcode 7) ; Less than
           (let* ([param1 (vector-ref memory (+ ip 1))]
                  [param2 (vector-ref memory (+ ip 2))]
                  [param3 (vector-ref memory (+ ip 3))]
                  [mode1 (list-ref modes 0)]
                  [mode2 (list-ref modes 1)]
                  [val1 (get-param-value memory param1 mode1)]
                  [val2 (get-param-value memory param2 mode2)]
                  [result (if (< val1 val2) 1 0)])
             (vector-set! memory param3 result)
             (execute (+ ip 4) outputs))]
          [(= opcode 8) ; Equals
           (let* ([param1 (vector-ref memory (+ ip 1))]
                  [param2 (vector-ref memory (+ ip 2))]
                  [param3 (vector-ref memory (+ ip 3))]
                  [mode1 (list-ref modes 0)]
                  [mode2 (list-ref modes 1)]
                  [val1 (get-param-value memory param1 mode1)]
                  [val2 (get-param-value memory param2 mode2)]
                  [result (if (= val1 val2) 1 0)])
             (vector-set! memory param3 result)
             (execute (+ ip 4) outputs))]
          [else
           (error "Unknown opcode" opcode)]))))
  (execute 0 '()))

(define (parse-instruction instruction)
  (let* ([opcode (modulo instruction 100)]
         [modes (quotient instruction 100)]
         [modes-list (parse-modes modes)])
    (values opcode modes-list)))

(define (parse-modes modes)
  (define (digits-reversed n)
    (if (= n 0)
        '()
        (cons (modulo n 10) (digits-reversed (quotient n 10)))))
  (let ([modes-list (digits-reversed modes)])
    (append modes-list (make-list (- 3 (length modes-list)) 0))))

(define (get-param-value memory param mode)
  (if (= mode 0)
      (vector-ref memory param)
      param))

; Main
(define memory (read-program))
(define outputs (execute-program memory))
(displayln outputs)
(displayln (last outputs))
