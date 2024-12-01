
(require rackunit)

;
; Advent of Code - 2017
;
; Part One
;
; --- Day 19: A Series of Tubes ---
;
; Somehow, a network packet got lost and ended up here. It's trying to follow
;  a routing diagram (your puzzle input), but it's confused about where to go.
;
; Its starting point is just off the top of the diagram. Lines (drawn with |,
;  -, and +) show the path it needs to take, starting by going down onto the
;  only line connected to the top of the diagram. It needs to follow this path
;  until it reaches the end (located somewhere within the diagram) and stop there.
;
; Sometimes, the lines cross over each other; in these cases, it needs to continue
;  going the same direction, and only turn left or right when there's no other option.
;  In addition, someone has left letters on the line; these also don't change its
;  direction, but it can use them to keep track of where it's been. For example:
;
;      |          
;      |  +--+    
;      A  |  C    
;  F---|----E|--+ 
;      |  |  |  D 
;      +B-+  +--+ 
;
; Given this diagram, the packet needs to take the following path:
;
; * Starting at the only line touching the top of the diagram, it must go
;    down, pass through A, and continue onward to the first +.
; * Travel right, up, and right, passing through B in the process.
; * Continue down (collecting C), right, and up (collecting D).
; * Finally, go all the way left through E and stopping at F.
;
; Following the path to the end, the letters it sees on its path are ABCDEF.
;
; The little packet looks up at you, hoping you can help it find the way. What
;  letters will it see (in the order it would see them) if it follows the path?
;  (The routing diagram is very wide; make sure you view it without line wrapping.)
;  (Answer: "HATBMQJYZ")
;

(define (load-tubes filename)
  (let ((tubes (make-hash))
        (lines (file->lines filename))
        (x 0) (y 0))
    (for ((line lines))
         (for ((ch (string->list line)))
              (when (not (char=? ch #\space))
                (hash-set! tubes (cons x y) ch)
                (when (and (zero? y) (char=? ch #\|))
                  (hash-set! tubes 'start (cons x y))))
              (set! x (add1 x)))
         (set! x 0)
         (set! y (add1 y)))
    tubes))

(define (find-new-dir tubes pos dir)
  (let ((new-dirs (case dir ((n s) '(e w)) ((e w) '(n s))))
        (exp-ch   (case dir ((n s) #\-)    ((e w) #\|)))
        (new-dir (cons 0 0)))
    (for ((test-dir new-dirs))
         (define test-pos (step test-dir pos))
         (when (hash-has-key? tubes test-pos)
           (let ((test-ch (hash-ref tubes test-pos)))
            (when (or (char=? test-ch exp-ch) (char-upper-case? test-ch))
              (set! new-dir test-dir)))))
    new-dir))

(define (step dir pos)
  (let ((x (car pos))
        (y (cdr pos)))
    (case dir
      ((n) (cons x (sub1 y)))
      ((s) (cons x (add1 y)))
      ((e) (cons (add1 x) y))
      ((w) (cons (sub1 x) y))
      (else (error (format "Unknown direction, dir: '~a'~n" dir))))))

(define (walk tubes)
  (let ((letters '())
        (dir 's)
        (step-cnt 0))
    (let loop ((pos (hash-ref tubes 'start)))
     (set! step-cnt (add1 step-cnt))
     (printf "Step count: ~a~n" step-cnt)
     (let* ((ch (hash-ref tubes pos))
            (new-pos (step dir pos)))

       (when (char-upper-case? ch) (set! letters (cons ch letters)))

       (when (char=? ch #\+)
         (set! dir (find-new-dir tubes pos dir))
         (loop (step dir pos)))

       (when (hash-has-key? tubes new-pos)
         (loop new-pos))
       ))
    (list->string (reverse letters))))

(check-equal? (walk (load-tubes "test-data.txt")) "ABCDEF")
(check-equal? (walk (load-tubes "day19-data.txt")) "HATBMQJYZ")

;
; --- Part Two ---
;
; The packet is curious how many steps it needs to go.
;
; For example, using the same routing diagram from the example above...
;
;      |          
;      |  +--+    
;      A  |  C    
;  F---|--|-E---+ 
;      |  |  |  D 
;      +B-+  +--+ 
;
; ...the packet would go:
;
; * 6 steps down (including the first line at the top of the diagram).
; * 3 steps right.
; * 4 steps up.
; * 3 steps right.
; * 4 steps down.
; * 3 steps right.
; * 2 steps up.
; * 13 steps left (including the F it stops on).
;
; This would result in a total of 38 steps.
;
; How many steps does the packet need to go? (Answer: 16332)
;

; Just added step-cnt and a printf to see the value.

