; Advent of Code 2015
;
; Day 1 - Not Quite Lisp
;
; An opening parenthesis, (, means he should go up one floor, and a closing
;    parenthesis, ), means he should go down one floor.
;
; (()) and ()() both result in floor 0.
; ((( and (()(()( both result in floor 3.
; ))((((( also results in floor 3.
; ()) and ))( both result in floor -1 (the first basement level).
; ))) and )())()) both result in floor -3.
;

(define count
  (lambda (item lst)
    (cond
      ((null? lst) 0)
      ((equal? item (car lst)) (add1 (count item (cdr lst))))
      (else (count item (cdr lst))))))

(assert (equal? 0 (count 'a '())))
(assert (equal? 2 (count 'a '( a b c a))))

(define test-day-1
  (lambda ()
    (assert (= 0 (which-floor "(())")))
    (assert (= 0 (which-floor "()()")))
    (assert (= 3 (which-floor "(((")))
    (assert (= 3 (which-floor "(()(()(")))
    (assert (= 3 (which-floor "))(((((")))
    (assert (= -1 (which-floor "())")))
    (assert (= -1 (which-floor "))(")))
    (assert (= -3 (which-floor ")))")))
    (assert (= -3 (which-floor ")())())")))))

(define which-floor
  (lambda (str)
    (- (count #\( (string->list str)) (count #\) (string->list str)))))

(test-day-1)

(define which-floor
  (lambda (str)
    (define tally
      (lambda (lst)
        (cond
          ((null? lst) 0)
          ((equal? #\( (car lst)) (add1 (tally (cdr lst))))
          ((equal? #\) (car lst)) (sub1 (tally (cdr lst))))
          (else (tally (cdr lst))))))
    (tally (string->list str))))

(test-day-1)

(define which-floor
  (lambda (str)
    (define tally
      (lambda (lst tot cnt)
        (if (= -1 tot) (format #t "Basement on cnt: ~d~%" cnt) #f) ; Position 1771
        (if (null? lst)
            tot
            (tally (cdr lst)
                   (+ tot
                      (case (car lst)
                        ((#\() 1)
                        ((#\)) -1)
                        (else 0)))
                   (add1 cnt)))))
    (tally (string->list str) 0 0)))

(test-day-1)

(define (read-line-from-file filename) ; Thank you bard.
  (call-with-input-file filename
                        (lambda (input-port)
                          (get-line input-port))))

(assert (= 138 (which-floor (read-line-from-file "day1-data.txt"))))

