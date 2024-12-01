; Advent of Code - 2015
;
; --- Part One ---
;
; Day 2: I Was Told There Would Be No Math
;
; The elves are running low on wrapping paper, and so they need to submit an order for more.
; They have a list of the dimensions (length l, width w, and height h) of each present,
; and only want to order exactly as much as they need.
; 
; Fortunately, every present is a box (a perfect right rectangular prism), which makes
; calculating the required wrapping paper for each gift a little easier: find the surface area
; of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also need a little extra paper for
; each present: the area of the smallest side.
; 
; For example:
; 
; A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper
;   plus 6 square feet of slack, for a total of 58 square feet.
; 
; A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper
;   plus 1 square foot of slack, for a total of 43 square feet.
; 
; All numbers in the elves' list are in feet.
; 
; How many total square feet of wrapping paper should they order? (Answer: 1606483)
;

(define take2-min
  (lambda lst
    (list-head (sort < lst) 2)))

(define paper
  (lambda (len width height)
    (+ (+ (* 2 len width) (* 2 width height) (* 2 height len))
       (apply * (take2-min len width height)))))

(assert (= 58 (paper 2 3 4)))
(assert (= 43 (paper 1 1 10)))


; --- Part Two ---
;
; The elves are also running low on ribbon. Ribbon is all the same width, so they only have
;  to worry about the length they need to order, which they would again like to be exact.
; 
; The ribbon required to wrap a present is the shortest distance around its sides, or the
;  smallest perimeter of any one face. Each present also requires a bow made out of ribbon
;  as well; the feet of ribbon required for the perfect bow is equal to the cubic feet of
;  volume of the present. Don't ask how they tie the bow, though; they'll never tell.
; 
; For example:
; 
; A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present
;  plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
;
; A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap the present
;  plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet.
;
; How many total feet of ribbon should they order? (Answer: 3842356)
;

(define ribbon
  (lambda (len width height)
    (let* ((min2 (take2-min len width height))
           (x (car min2))
           (y (cadr min2)))
      (+ (+ (* 2 x) (* 2 y))
         (* len width height)))))

(assert (= 34 (ribbon 2 3 4)))
(assert (= 14 (ribbon 1 1 10)))


; Thank you Bard for make-accumulator.

(define (make-accumulator)
  (let ((total 0))
   (lambda (action . args)
     (cond ((eq? action 'add)
            (set! total (+ total (car args))))
           ((eq? action 'get-total)
            total)
           ((eq? action 'reset)
            (set! total 0)
            'total-reset)
           (else
             (error "Invalid action: ~a" action))))))


; Thank you ChatGPT-4 for string-to-number-list, string-split and string-index.

(define (string-to-number-list str)
  (map string->number (string-split str #\x)))

(define (string-split str char)
  (let loop ((s str) (start 0) (result '()))
   (let ((pos (string-index s char start)))
    (if pos
        (loop s (+ pos 1)
              (cons (substring s start pos) result))
        (reverse (cons (substring s start (string-length s)) result))))))

(define (string-index str char start)
  (let loop ((i start))
   (cond ((>= i (string-length str)) #f)
         ((char=? (string-ref str i) char) i)
         (else (loop (+ i 1))))))

(assert (equal? '(1 2 3) (string-to-number-list "1x2x3")))


; Thank you ChatGPT4 for all of this code from here on:

(define (process-file-lines filename process-fn)
  (let ((content (call-with-input-file filename get-string-all)))
   (let loop ((lines (string-split content #\newline)))
    (unless (null? lines)
      (process-fn (car lines))
      (loop (cdr lines))))))

(define (string-split str delimiter)
  (letrec ((split
             (lambda (str)
               (let ((pos (string-index str delimiter)))
                (if pos
                    (cons (substring str 0 pos)
                          (split (substring str (+ pos 1) (string-length str))))
                    (list str))))))
    (split str)))

(define (string-index str delimiter)
  (let loop ((i 0))
   (cond ((>= i (string-length str)) #f)
         ((char=? (string-ref str i) delimiter) i)
         (else (loop (+ i 1))))))

(define (example-process-fn line)
  (let* ((dimensions (string-to-number-list line))
         (p-value (apply paper dimensions))
         (r-value (apply ribbon dimensions)))
;    (format #t "~a -> ~d, ~d~%" line p-value r-value)
    (paper-accumulator 'add p-value)
    (ribbon-accumulator 'add r-value)))

(define paper-accumulator (make-accumulator))
(define ribbon-accumulator (make-accumulator))

(process-file-lines "day2-data.txt" example-process-fn)
(format #t "Total paper: ~d~%" (paper-accumulator 'get-total)) ; Total paper: 1606483
(format #t "Total ribbon: ~d~%" (ribbon-accumulator 'get-total)) ; Total ribbon: 3842356

