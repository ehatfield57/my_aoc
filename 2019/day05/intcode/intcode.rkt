#lang racket

(require rackunit)

(provide make-intcode-interpreter intcode-tests)

; * Opcode 1 adds together numbers read from two positions and stores the result in
;    a third position. The three integers immediately after the opcode tell you
;    these three positions - the first two indicate the positions from which you
;    should read the input values, and the third indicates the position at which
;    the output should be stored.
;
; * Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead
;    of adding them. Again, the three integers after the opcode indicate where the
;    inputs and outputs are, not their values.
;
; * Opcode 3 takes a single integer as input and saves it to the position
;    given by its only parameter. For example, the instruction 3,50 would
;    take an input value and store it at address 50.
;
; * Opcode 4 outputs the value of its only parameter. For example, the
;    instruction 4,50 would output the value at address 50.
;
; * Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the
;    instruction pointer to the value from the second parameter. Otherwise,
;    it does nothing.
;
; * Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
;    instruction pointer to the value from the second parameter. Otherwise,
;    it does nothing.
;
; * Opcode 7 is less than: if the first parameter is less than the second
;    parameter, it stores 1 in the position given by the third parameter.
;    Otherwise, it stores 0.
;
; * Opcode 8 is equals: if the first parameter is equal to the second parameter,
;  it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
;

; (extract-digits num pos) where num: 1002
;                                pos: 4321
(define (extract-digits num pos)
  (modulo (truncate (/ num (expt 10 (sub1 pos)))) 10))

; (get-values 1002) ABCDE
;                    1002
; DE - Two-digit opcode,      02 == opcode 2
;  C - Mode of 1st parameter,  0 == position mode
;  B - Mode of 2nd parameter,  1 == immediate mode
;  A - Mode of 3rd parameter,  0 == position mode
(define (get-values num)
  (list
    (+ (* 10 (extract-digits num 2)) (extract-digits num 1))
    (extract-digits num 3)
    (extract-digits num 4)
    (extract-digits num 5)))

(define (load-numbers numbers)
   (list->vector (map string->number (string-split (string-trim numbers) ","))))

(define (get-value program mode ptr)
  (if (zero? mode)
      (vector-ref program (vector-ref program ptr))
      (vector-ref program ptr)))

(define (set-value! program ptr value)
  (vector-set! program (vector-ref program ptr) value))

(define (ic-addition program pointer-idx modes)
  (define-values (mode1 mode2 mode3) (apply values modes))
  (let ((value (+ (get-value program mode1 (+ 1 pointer-idx))
                  (get-value program mode2 (+ 2 pointer-idx)))))
    (set-value! program (+ 3 pointer-idx) value)) )

(define (ic-multiply program pointer-idx modes)
  (define-values (mode1 mode2 mode3) (apply values modes))
  (let ((value (* (get-value program mode1 (+ 1 pointer-idx))
                  (get-value program mode2 (+ 2 pointer-idx)))))
    (set-value! program (+ 3 pointer-idx) value)))

(define (execute program (io-queue '()))
  (let ((io-push (lambda (val) (set! io-queue (cons val io-queue))))
        (io-pop (lambda () (let ((the-end (last io-queue)))
                            (set! io-queue (reverse (drop (reverse io-queue) 1)))
                            the-end))))
    (let loop ((program-idx 0))
     (define modes (get-values (vector-ref program program-idx)))
     (case (car modes)
       ((99)
        program)
       ((1) ; Add (3 parameters)
        (ic-addition program program-idx (cdr modes))
        (loop (+ 4 program-idx)))

       ((2) ; Multiply (3 parameters)
        (ic-multiply program program-idx (cdr modes))
        (loop (+ 4 program-idx)))

       ((3) ; Input (1 parameter)
        (set-value! program (+ 1 program-idx) (io-pop))
        (loop (+ 2 program-idx)))

       ((4) ; Output (1 parameter)
        (io-push (get-value program 0 (+ 1 program-idx)))
        (loop (+ 2 program-idx)))

       ((5) (printf "TBD~n"))
       ((6) (printf "TBD~n"))
       ((7) (printf "TBD~n"))
       ((8) (printf "TBD~n"))

       (else (error (format "Error in Intcode program: ~a~n" (vector-ref program program-idx)))))))
  io-queue)

(define (make-intcode-interpreter numbers)
  (let* ((original-program (load-numbers numbers))
         (program (vector-copy original-program))
         (io-queue '())
         (instruction-ptr 0))
    (lambda (cmd . args)
      (case cmd
        ((reset)
         (set! program (vector-copy original-program))
         (set! io-queue '())
         (set! instruction-ptr 0))

        ((execute) (set! io-queue (execute program io-queue)))
        ((dump)    (string-join (map number->string (vector->list program)) ","))

        ((get-io)  io-queue)
        ((push-io) (set! io-queue (cons (car args) io-queue)))
        ((pop-io)
         (define value (car (reverse io-queue)))
         (set! io-queue (reverse (drop (reverse io-queue) 1)))
         value)

        ; Debugging functions
        ((step) "TBD")
        ((show) "TBD")

        (else
          (error (format "No such intcode interpreter command: '~a'~n" cmd)))))))

(define (test-program intcode-str)
  (let ((ic-test (make-intcode-interpreter intcode-str)))
   (ic-test 'execute)
   (ic-test 'dump)))

(define (test-io intcode-str input-value)
  (let ((ic-test (make-intcode-interpreter intcode-str)))
   (ic-test 'push-io input-value)
   (ic-test 'execute)
   (ic-test 'get-io)))

(define (intcode-tests)
  (check-equal? (test-program "1,0,0,0,99") "2,0,0,0,99")
  (check-equal? (test-program "2,3,0,3,99") "2,3,0,6,99")
  (check-equal? (test-program "2,4,4,5,99,0") "2,4,4,5,99,9801")
  (check-equal? (test-program "1,1,1,4,99,5,6,0,99") "30,1,1,4,2,5,6,0,99")
  (check-equal? (test-program "1,9,10,3,2,3,11,0,99,30,40,50") "3500,9,10,70,2,3,11,0,99,30,40,50")
  (check-equal? (test-program "1002,4,3,4,33") "1002,4,3,4,99")
  (check-equal? (test-program "1101,100,-1,4,0") "1101,100,-1,4,99")

  (check-equal? (test-io "3,0,4,0,99" 1) '(1))
  (check-equal? (test-io "3,0,4,0,99" 2) '(2)))


