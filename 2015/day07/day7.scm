
(require rackunit)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 7: Some Assembly Required ---
;
; This year, Santa brought little Bobby Tables a set of wires and bitwise logic
;  gates! Unfortunately, little Bobby is a little under the recommended age range,
;  and he needs help assembling the circuit.
; 
; Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal
;  (a number from 0 to 65535). A signal is provided to each wire by a gate, another wire,
;  or some specific value. Each wire can only get a signal from one source, but can
;  provide its signal to multiple destinations. A gate provides no signal until all of
;  its inputs have a signal.
; 
; The included instructions booklet describes how to connect the parts together:
;  x AND y -> z means to connect wires x and y to an AND gate, and then connect its
;  output to wire z.
; 
; For example:
; 
; * 123 -> x means that the signal 123 is provided to wire x.
;
; * x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
;
; * p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
;
; * NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.
;
; Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some reason,
;  you'd like to emulate the circuit instead, almost all programming languages (for example,
;  C, JavaScript, or Python) provide operators for these gates.
; 
; For example, here is a simple circuit:
; 
; 123 -> x
; 456 -> y
; x AND y -> d
; x OR y -> e
; x LSHIFT 2 -> f
; y RSHIFT 2 -> g
; NOT x -> h
; NOT y -> i
;
; After it is run, these are the signals on the wires:
; 
; d: 72
; e: 507
; f: 492
; g: 114
; h: 65412
; i: 65079
; x: 123
; y: 456
;
; In little Bobby's kit's instructions booklet (provided as your puzzle input),
;  what signal is ultimately provided to wire a? (Answer: 3176)
; 

(define mask-16-bits #xFFFF)
(define (mask x) (bitwise-and mask-16-bits x))

(define (b-and x y) (mask (bitwise-and x y)))
(define (b-or  x y) (mask (bitwise-ior x y)))
(define (b-xor x y) (mask (bitwise-xor x y)))
(define (b-not x)   (mask (bitwise-not x)))

(define (rshift x n) (mask (arithmetic-shift x (- n))))
(define (lshift x n) (mask (arithmetic-shift x n)))

(define x 123)
(define y 456)
(check-equal? 72 (b-and x y))
(check-equal? 507 (b-or x y))
(check-equal? 492 (lshift x 2))
(check-equal? 114 (rshift y 2))
(check-equal? 65412 (b-not x))
(check-equal? 65079 (b-not y))

(define functions (make-hash))
(define f-values (make-hash))

(define (input-line line)
  (let* ((main-parts (regexp-match #rx"^(.*?) -> ([a-z]+)$" line))
          (func (cadr  main-parts))
          (wire (caddr main-parts))
          (rx-wire   (regexp-match #rx"^[a-z]+$" func))
          (rx-number (regexp-match #rx"^[0-9]+$" func))
          (rx-not (regexp-match #rx"^NOT ([a-z]+)$" func))
          (rx-and (regexp-match #rx"^([a-z0-9]+) AND ([a-z]+)$" func))
          (rx-or (regexp-match #rx"^([a-z0-9]+) OR ([a-z]+)$" func))
          (rx-lshift (regexp-match #rx"^([a-z]+) LSHIFT ([0-9]+)$" func))
          (rx-rshift (regexp-match #rx"^([a-z]+) RSHIFT ([0-9]+)$" func)))
    (cond
      ((list? rx-wire)          (hash-set! f-values wire rx-wire))
      ((not (false? rx-number)) (hash-set! f-values wire (string->number func)))
      ((list? rx-not)    (hash-set! functions wire (list b-not (cadr rx-not))))
      ((list? rx-and)    (hash-set! functions wire (list b-and (cdr rx-and))))
      ((list? rx-or)     (hash-set! functions wire (list b-or (cdr rx-or))))
      ((list? rx-lshift) (hash-set! functions wire (list lshift (cdr rx-lshift))))
      ((list? rx-rshift) (hash-set! functions wire (list rshift (cdr rx-rshift))))
      (else (error (string-append "Unknown line in input-line: " line))))))

(define (solve-for wire)
  (cond
    ((number? wire) wire)
    ((and (string? wire) (regexp-match #rx"^[0-9]+$" wire)) (string->number wire))
    ((and (hash-has-key? f-values wire))
     (let ((f-value (hash-ref f-values wire)))
      (cond
        ((number? f-value) f-value)
        ((and (string? f-value) (regexp-match #rx"^[0-9]+" f-value)) (string->number f-value))
        (else (solve-for (car f-value))))))
    (else
      (let ((function (hash-ref functions wire)))
       (hash-set! f-values wire
                  (apply (car function)
                         (map solve-for
                              (if (list? (cadr function))
                                  (cadr function)
                                  (cdr function)))))
       (hash-ref f-values wire)))))

(define (part-one filename)
  (hash-clear! f-values)
  (hash-clear! functions)

  (with-input-from-file
    filename
    (lambda ()
      (for ([line (in-lines)])
        (input-line line))))
  (solve-for "a"))

(check-equal? 3176 (part-one "day7-data.txt"))

;
; --- Part Two ---
;
; Now, take the signal you got on wire a, override wire b to that signal,
;  and reset the other wires (including wire a). What new signal is
;  ultimately provided to wire a? (Answer: 14710)

(define (part-two filename)
  (hash-clear! f-values)
  (hash-clear! functions)

  (with-input-from-file
    filename
    (lambda ()
      (for ([line (in-lines)])
        (input-line line))))
  (hash-set! f-values "b" 3176)
  (solve-for "a"))

(check-equal? 14710 (part-two "day7-data.txt"))

