
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
;
; Advent of Code - 2018
;
; Part One
;
; --- Day 6: Chronal Coordinates ---
;
; The device on your wrist beeps several times, and once again you feel
;  like you're falling.
;
; "Situation critical," the device announces. "Destination indeterminate.
;  Chronal interference detected. Please specify new target coordinates."
;
; The device then produces a list of coordinates (your puzzle input). Are
;  they places it thinks are safe or dangerous? It recommends you check
;  manual page 729. The Elves did not give you a manual.
;
; If they're dangerous, maybe you can minimize the danger by finding the
;  coordinate that gives the largest distance from the other points.
;
; Using only the Manhattan distance, determine the area around each coordinate
;  by counting the number of integer X,Y locations that are closest to that
;  coordinate (and aren't tied in distance to any other coordinate).
;
; Your goal is to find the size of the largest area that isn't infinite. For
;  example, consider the following list of coordinates:
;
; 1, 1
; 1, 6
; 8, 3
; 3, 4
; 5, 5
; 8, 9
;
; If we name these coordinates A through F, we can draw them on a grid, putting
;  0,0 at the top left:
;
; ..........
; .A........
; ..........
; ........C.
; ...D......
; .....E....
; .B........
; ..........
; ..........
; ........F.
;
; This view is partial - the actual grid extends infinitely in all directions.
;  Using the Manhattan distance, each location's closest coordinate can be determined,
;  shown here in lowercase:
;
; aaaaa.cccc
; aAaaa.cccc
; aaaddecccc
; aadddeccCc
; ..dDdeeccc
; bb.deEeecc
; bBb.eeee..
; bbb.eeefff
; bbb.eeffff
; bbb.ffffFf
;
; Locations shown as . are equally far from two or more coordinates, and so they
;  don't count as being closest to any.
;
; In this example, the areas of coordinates A, B, C, and F are infinite - while
;  not shown here, their areas extend forever outside the visible grid. However,
;  the areas of coordinates D and E are finite: D is closest to 9 locations, and
;  E is closest to 17 (both including the coordinate's location itself). Therefore,
;  in this example, the size of the largest area is 17.
;
; What is the size of the largest area that isn't infinite? (Answer: ?)
;

; (define (get-points filename)
;   (let ((lines (file->lines filename))
;         (points (make-hash))
;         (id 1))
;    (for ((line lines))
;         (define-values (x y) (apply values (map string->number (regexp-match* #rx"([0-9]+)" line))))
;         (hash-set! points (cons x y) id)
;         (set! id (add1 id)))
;    points))

(defun get-points (filename)
  (let ((points (make-hash-table))  ; Create a hash table
        (id 1))                     ; Initialize ID counter
    (with-open-file (stream filename) ; Open the file
      (loop for line = (read-line stream nil) ; Read lines until end of file
            while line
            do (multiple-value-bind (x y)  ; Parse x and y using destructuring-bind
                 (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" line)) ; Extract numbers using regular expression
                 (setf (gethash (cons x y) points) id)  ; Store (x, y) as key with ID as value
                 (incf id))))                            ; Increment ID
    points))


; (define (get-size points)
;   (let ((min-x 9999)
;         (min-y 9999)
;         (max-x 0)
;         (max-y 0))
;     (for ((coor (hash-keys points)))
;          (let ((x (car coor))
;                (y (cdr coor)))
;            (when (< x min-x) (set! min-x x))
;            (when (> x max-x) (set! max-x x))
;            (when (< y min-y) (set! min-y y))
;            (when (> y max-y) (set! max-y y))))
;     (values min-x min-y max-x max-y)))

(defun get-size (points)
  (let ((min-x most-positive-fixnum) ; Initialize to the largest possible fixnum
        (min-y most-positive-fixnum)
        (max-x most-negative-fixnum) ; Initialize to the smallest possible fixnum
        (max-y most-negative-fixnum))
    (maphash  ; Iterate over key-value pairs in the hash table
      (lambda (key value)
        (destructuring-bind (x y) key ; Destructure the (x, y) pair
          (when (< x min-x) (setf min-x x))
          (when (> x max-x) (setf max-x x))
          (when (< y min-y) (setf min-y y))
          (when (> y max-y) (setf max-y y))))
      points)  
    (values min-x min-y max-x max-y))) ; Return multiple values

; (define (draw-points points)
;   (define-values (min-x min-y max-x max-y) (get-size points))
;   (let ((alphabet (list->vector (map string (string->list " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))))
;     (for ((y (range (sub1 min-y) (+ 2 max-y))))
;          (for ((x (range (sub1 min-x) (+ 2 max-x))))
;               (let ((coor (cons x y)))
;                 (printf "~a" (cond
;                                ((and (hash-has-key? points coor) (zero? (hash-ref points coor))) ".")
;                                ((hash-has-key? points coor) (vector-ref alphabet (hash-ref points coor)))
;                                (else "-")))))
;          (printf "~n"))
;     (printf "~n")))

(defun draw-points (points)
  (multiple-value-bind (min-x min-y max-x max-y) (get-size points) ; Get min/max values
    (let ((alphabet (make-array 63 :element-type 'character :initial-contents " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))) ; Create character vector
      (loop for y from (1- min-y) to (+ 2 max-y)
            do (loop for x from (1- min-x) to (+ 2 max-x)
                     do (let ((coor (cons x y)))
                          (format t "~a" (cond 
                                           ((and (gethash coor points) (zerop (gethash coor points))) ".")
                                           ((gethash coor points) (aref alphabet (gethash coor points)))
                                           (t "-"))))) ; Format output
            (format t "~%"))))) ; Newline after each row

; (define (available-points points x y)
;   (let ((north (cons x (sub1 y)))
;         (south (cons x (add1 y)))
;         (east  (cons (add1 x) y))
;         (west  (cons (sub1 x) y))
;         (result '())
;         (avail? (lambda (coor) (not (hash-has-key? points coor)))))
;     (when (avail? north) (set! result (cons north result)))
;     (when (avail? south) (set! result (cons south result)))
;     (when (avail? east)  (set! result (cons east result)))
;     (when (avail? west)  (set! result (cons west result)))
;     result))

(defun available-points (points x y)
  (let ((north (cons x (1- y)))
        (south (cons x (1+ y)))
        (east  (cons (1+ x) y))
        (west  (cons (1- x) y))
        (result nil))  ; Start with an empty list
    (flet ((avail? (coor) (not (gethash coor points)))) ; Define local function
      (when (avail? north) (push north result))
      (when (avail? south) (push south result))
      (when (avail? east)  (push east result))
      (when (avail? west)  (push west result)))
    (nreverse result))) ; Reverse the list for correct order

; (define (step-points points)
;   (let ((keys (filter (lambda (coor) (not (zero? (hash-ref points coor)))) (hash-keys points)))
;         (stamp! (lambda (coor n) (cond
;                                    ((not (hash-has-key? points coor)) (hash-set! points coor n))
;                                    ((not (zero? (hash-ref points coor))) (hash-set! points coor 0))))))
;     (for ((key keys))
;          (for ((coor (available-points points (car key) (cdr key))))
;               (stamp! coor (hash-ref points key))))))

;; (defun step-points (points)
;;   (let ((keys (loop for key being the hash-keys of points 
;;                     using (hash-value value)
;;                     when (not (zerop value)) 
;;                     collect key))) ; Filter keys with non-zero values
;;     (maphash  ; Iterate over filtered keys
;;       (lambda (key value) 
;;         (loop for coor in (available-points points (car key) (cdr key)) ; Iterate over available points
;;               do (if (not (gethash coor points))
;;                      (setf (gethash coor points) value)  ; Set value if not present
;;                      (when (not (zerop (gethash coor points))) ; Set to 0 if not zero
;;                        (setf (gethash coor points) 0)))))
;;       points)))

(defun step-points (points)
  (let ((keys (loop for key being the hash-keys of points 
                    using (hash-value value)
                    when (not (zerop value)) 
                    collect key))) ; Filter keys with non-zero values
    (maphash  ; Iterate over filtered keys
     (lambda (key value) 
       (loop for coor in (available-points points (car key) (cdr key)) ; Iterate over available points
             do (if (not (gethash coor points))
                    (setf (gethash coor points) value)  ; Set value if not present
                    (when (not (zerop (gethash coor points))) ; Handle conflict
                      (let ((winner (alexandria:random-elt (list key coor)))) ; Randomly choose winner
                        (setf (gethash (cdr winner) points) (gethash (car winner) points)))))))
     points)))

; (define (total-points points)
;   (let ((tally (make-hash)))
;     (for ((coor (hash-keys points)))
;          (let ((val (hash-ref points coor)))
;            (if (hash-has-key? tally val)
;                (hash-set! tally val (add1 (hash-ref tally val)))
;                (hash-set! tally val 1))))
;     tally))

(defun total-points (points)
  (let ((tally (make-hash-table))) ; Create the tally hash table
    (maphash  ; Iterate over points hash table
      (lambda (key value)
        (incf (gethash value tally 0))) ; Increment count for the value
      points)
    tally))  ; Return the tally hash table

; (define (part-one filename (iters 5))
;   (let ((points (get-points filename)))
;     ;   (draw-points points)
;     (for ((i (range iters)))
;          (step-points points)
;          ;        (draw-points points)
;          (printf "Total values in step ~a: ~a~n" i (total-points points)))))

(define (part-one filename (iters 5))
  (let ((points (get-points filename)))
    (draw-points points)
    (for ((i (range iters)))
         (step-points points)
         (draw-points points)
         (printf "Total values in step ~a: ~a~n" i (total-points points)))))

; 3435 is too high :-(

