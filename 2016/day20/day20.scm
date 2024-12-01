
(require rackunit)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 20: Firewall Rules ---
;
; You'd like to set up a small hidden computer here so you can use it to
;  get back into the network later. However, the corporate firewall only
;  allows communication with certain external IP addresses.
;
; You've retrieved the list of blocked IPs from the firewall, but the list
;  seems to be messy and poorly maintained, and it's not clear which IPs
;  are allowed. Also, rather than being written in dot-decimal notation,
;  they are written as plain 32-bit integers, which can have any value
;  from 0 through 4294967295, inclusive.
;
; For example, suppose only the values 0 through 9 were valid, and that
;  you retrieved the following blacklist:
;
; 5-8
; 0-2
; 4-7
;
; The blacklist specifies ranges of IPs (inclusive of both the start and end
;  value) that are not allowed. Then, the only IPs that this firewall allows
;  are 3 and 9, since those are the only numbers not in any range.
;
; Given the list of blocked IPs you retrieved from the firewall (your puzzle
;  input), what is the lowest-valued IP that is not blocked?  (Answer: 17348574)
;

; Part One answer done by hand (see thinking.txt) by sorting the ranges.

;
; --- Part Two ---
;
; How many IPs are allowed by the blacklist?  (Answer: 104)
;

(define (get-min-max line)
  (define-values (min-num max-num) (apply values (car (regexp-match* #rx"([0-9]+)-([0-9]+)" line #:match-select cdr))))
  (cons (string->number min-num) (string->number max-num)))

(define (within num min-max)
  (and (>= num (car min-max)) (<= num (cdr min-max))))

(define (get-min-maxs filename)
  (let ((lines (file->lines filename))
        (min-maxs '()))
    (for ((line lines))
         (set! min-maxs (cons (get-min-max line) min-maxs)))
    (sort min-maxs < #:key car)))

; Just a comment: The Max number in the data is '4,294,967,295'.

(define (part-two filename)
  (let ((groups (get-min-maxs filename))
        (groupings (list (cons 0 4294967295)))
        (max-max 99)
        (min-min 99))
    (for ((group groups))
         (let ((min-num (car group))
               (max-num (cdr group))
               (new-groupings '()))
           (when (< min-num min-min) (set! min-min min-num))
           (when (> max-num max-max) (set! max-max max-num))

           (for ((free-grp groupings))
                (cond

                  ((and (within min-num free-grp) (within max-num free-grp))
                   (set! new-groupings (cons (cons (add1 max-num) (cdr free-grp)) new-groupings)) 
                   (when (not (= (car free-grp) min-num))
                     (set! new-groupings (cons (cons (car free-grp) (sub1 min-num)) new-groupings))))

                  ((within min-num free-grp)
                   (set! new-groupings (cons (cons (car free-grp) (sub1 min-num)) new-groupings))
                   )

                  ((within max-num free-grp)
                   (set! new-groupings (cons (cons (add1 max-num) (cdr free-grp)) new-groupings)))

                  (else
                    (set! new-groupings (cons free-grp new-groupings)))))

           (set! groupings new-groupings)))
    (let ((total 0))
     (for ((group groupings))
          (for ((i (range (car group) (add1 (cdr group)))))
               (set! total (add1 total))))
     (printf "Total free IPs: ~a~n" total)
     total)))

(check-equal? (part-two "day20-data.txt") 104)

