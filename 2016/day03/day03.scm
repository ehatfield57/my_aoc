
(require rackunit)
(require racket/trace)

;
; Advent of Code - 2016
;
; Part One
;
; --- Day 3: Squares With Three Sides ---
;
; Now that you can think clearly, you move deeper into the labyrinth of hallways and
;  office furniture that makes up this part of Easter Bunny HQ. This must be a graphic
;  design department; the walls are covered in specifications for triangles.
;
; Or are they?
;
; The design document gives the side lengths of each triangle it describes, but... 5
;  10 25? Some of these aren't triangles. You can't help but mark the impossible ones.
;
; In a valid triangle, the sum of any two sides must be larger than the remaining side.
;  For example, the "triangle" given above is impossible, because 5 + 10 is not larger
;  than 25.
;
; In your puzzle input, how many of the listed triangles are possible?  (Answer: ?)
;

(define (possible? x y z)
  (cond
    ((<= (+ x y) z) #f)
    ((<= (+ y z) x) #f)
    ((<= (+ z x) y) #f)
    (else #t)))

(check-equal? (possible? 5 10 25) #f) ; test data

(define real-data (file->lines "day03-data.txt"))

(define (get-possible data)
  (filter
    (lambda (line)
      (define-values (x y z) (apply values (map string->number (string-split line))))
      (possible? x y z))
    data))

(printf "Part One: The number of valid triangles: ~a~n" (length (get-possible real-data)))


;
; --- Part Two ---
;
; Now that you've helpfully marked up their design documents, it occurs to you that
;  triangles are specified in groups of three vertically. Each set of three numbers
;  in a column specifies a triangle. Rows are unrelated.
;
; For example, given the following specification, numbers with the same hundreds digit
;  would be part of the same triangle:
;
; 101 301 501
; 102 302 502
; 103 303 503
; 201 401 601
; 202 402 602
; 203 403 603
;
; In your puzzle input, and instead reading by columns, how many of the listed triangles
;  are possible? (Answer: 1921)
;

(define test-data '(
                    " 101 301 501"
                    " 102 302 502"
                    " 103 303 503"
                    " 201 401 601"
                    " 202 402 602"
                    " 203 403 603"))

(define right-results '(
                        " 101 102 103"
                        " 301 302 303"
                        " 501 502 503"
                        " 201 202 203"
                        " 401 402 403"
                        " 601 602 603"))

(define (reorder-data data)
  (cond
    ((null? data) '())
    (else
      (define-values (a1 b1 c1) (apply values (map string->number (string-split (car data)))))
      (define-values (a2 b2 c2) (apply values (map string->number (string-split (cadr data)))))
      (define-values (a3 b3 c3) (apply values (map string->number (string-split (caddr data)))))
      (cons (format " ~a ~a ~a" a1 a2 a3)
            (cons (format " ~a ~a ~a" b1 b2 b3)
                  (cons (format " ~a ~a ~a" c1 c2 c3)
                        (reorder-data (cdddr data))))))))

(check-equal? (reorder-data test-data) right-results)

(printf "Part Two: The number of valid triangles: ~a~n" (length (get-possible (reorder-data real-data))))

