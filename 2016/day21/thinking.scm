
; (String length of 5 doesn't work)
;  (because reversing formula has zero)
;  (as the result of two distince input)
;  (values.  But the Eight should be ok)
; 
;  (Str Length: 8)      new mod
; idx: x -> +? -> rr    pos len
; ---- -    --    --    --- ---
;   0  0 -> +1 ->  1 ->  1   1
;   1  1 -> +1 ->  2 ->  3   3  -> ?? -> 1
;   2  2 -> +1 ->  3 ->  5   5
;   3  3 -> +1 ->  4 ->  7   7
;   4  4 -> +2 ->  6 -> 10   2
;   5  5 -> +2 ->  7 -> 12   4
;   6  6 -> +2 ->  8 -> 14   6
;   7  7 -> +2 ->  9 -> 16   0
; 
; Just use an assoc lookup:
; (
;   (0 . 7) (1 . 0) (2 . 4) (3 . 1) (4 . 5) (5 . 2) (6 . 6) (7 . 3)
; )

(define (rotate-left letters)
  (let ((temp (vector-ref letters 0)))
   (for ((i (range (sub1 (vector-length letters)))))
        (vector-set! letters i (vector-ref letters (add1 i))))
   (vector-set! letters (sub1 (vector-length letters)) temp)))

(define (rotate-right letters)
  (let* ((max-idx (sub1 (vector-length letters)))
         (temp (vector-ref letters max-idx)))
    (do ((i max-idx (sub1 i)))
        ((< i 1) #t)
        (vector-set! letters i (vector-ref letters (sub1 i))))
    (vector-set! letters 0 temp)))

(define (rotate-cmd letters x dir)
  (for ((i (range x)))
       (case dir
         ('left  (rotate-left letters))
         ('right (rotate-right letters))
         (else (error "rotate command dir not left or right!")))))

;(define (undoit letters x)
;  (define foo (index-of (vector->list letters) (string-ref x 0)))
;  (define bar (vector-ref (vector 7 0 4 1 3 2 4 3) foo))
;  (define pos (- (max foo bar) (min foo bar)))
;  (rotate-cmd letters pos 'left)
;  letters)

(define (undoit letters x)
  (define foo (index-of (vector->list letters) (string-ref x 0)))
  (define pos (vector-ref (vector 1 1 6 2 7 3 0 4) foo))
  (printf " - foo: ~a - pos: ~a " foo pos)
  (rotate-cmd letters pos 'left)
  letters)

(define (doit x)
  (let ((letters (list->vector (string->list "abcdefgh"))))
   (printf "Was ~a" letters)
   (define pos (add1 (index-of (vector->list letters) (string-ref x 0))))
   (when (>= pos 5) (set! pos (add1 pos))) ; Because I've added 1 to it already
   (printf ", with x: '~a' and pos: ~a" x pos)
   (rotate-cmd letters pos 'right)
   (printf " -> ~a" letters)
   (printf " -> ~a~n" (undoit letters x))
   pos))

(doit "a")
(doit "b")
(doit "c")
(doit "d")
(doit "e")
(doit "f")
(doit "g")
(doit "h")

