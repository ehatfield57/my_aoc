
(require rackunit)
(require graph)

(struct unit (type alive attack hps) #:transparent #:mutable)

(define new-goblin (unit 'goblin  #t 3 200))
(define new-elf    (unit 'elf     #t 3 200))

(define (reading-order p1 p2)
  (cond
    [(< (cdr p1) (cdr p2)) #t] 
    [(> (cdr p1) (cdr p2)) #f] 
    [else (< (car p1) (car p2))]))

(define (load-cave filename)
  (let ((cave (make-hash))
        (units (make-hash))
        (input (open-input-file filename))
        (x 0) (y 0))
    (let loop ((ch (read-char input)))
     (case ch
       ((#\# #\.)
        (hash-set! cave (cons x y) ch)
        (set! x (add1 x)))
       ((#\G)
        (hash-set! units (cons x y) new-goblin)
        (hash-set! cave (cons x y) #\.)
        (set! x (add1 x)))
       ((#\E)
        (hash-set! units (cons x y) new-elf)
        (hash-set! cave (cons x y) #\.)
        (set! x (add1 x)))
       ((#\newline)
        (set! y (add1 y)) 
        (set! x 0))
       )
     (set! ch (read-char input))
     (when (not (eof-object? ch)) (loop ch)))
    (close-input-port input)

    (values cave units)))

(define (draw-cave cave units (highlights '()))
  (let ((max-x (apply max (map car (hash-keys cave))))
        (max-y (apply max (map cdr (hash-keys cave)))))
    (for ((y (range (add1 max-y))))
         (for ((x (range (add1 max-x))))
              (if (hash-has-key? units (cons x y))
                  (cond
                    ((eq? (unit-type (hash-ref units (cons x y))) 'goblin) (printf "G"))
                    ((eq? (unit-type (hash-ref units (cons x y))) 'elf) (printf "E")))
                  (cond
                    ((member (cons x y) highlights) (printf "+"))
                    (else (printf "~a" (hash-ref cave (cons x y)))))))
         (printf "~n"))
    (printf "~n")))

(define (get-units units type)
  (let ((return-units '()))
   (for ((coor (hash-keys units)))
        (define unit (hash-ref units coor))
        (when (equal? (unit-type unit) type)
          (set! return-units (cons unit return-units))))
   return-units))

(define (get-target-coors unit units)
  (let* ((return-coors '())
         (enemy-type (if (equal? (unit-type unit) 'elf) 'goblin 'elf)))
    (for ((coor (hash-keys units)))
         (when (equal? (unit-type (hash-ref units coor)) enemy-type)
           (set! return-coors (cons coor return-coors))))
    return-coors))

(define (get-nsew-coors coor)
  (let ((x (car coor))
        (y (cdr coor)))
  (list (cons       x (sub1 y))
        (cons       x (add1 y))
        (cons (add1 x)      y)
        (cons (sub1 x)      y))))

(define (coor-empty? coor cave units)
  (and (hash-has-key? cave coor)
       (char=? #\. (hash-ref cave coor))
       (not (hash-has-key? units coor))))

(define (get-in-range-coors unit-coor target-coors cave units)
  (let* ((in-range-coors '()))
         (for ((t-coor target-coors))
              (let ((nsew (get-nsew-coors t-coor)))
               (for ((nsew-coor nsew))
                    (when (coor-empty? nsew-coor cave units)
                      (set! in-range-coors (cons nsew-coor in-range-coors))))))
    in-range-coors))

(define (get-available-floor-coors cave units)
  (let ((coors '())
        (unit-coors (hash-keys units)))
    (for ((coor (hash-keys cave)))
         (when (and (char=? #\. (hash-ref cave coor))
                    (not (member coor unit-coors)))
           (set! coors (cons coor coors))))
    coors))

(define (convert-to-graph available-floor-coors unit-coor)
  (let ((lookup (make-hash))
        (vertices '()))

    (hash-set! lookup unit-coor '())
    (for ((coor available-floor-coors)) ; #hash((1 . 1) => '(), ...)
         (hash-set! lookup coor '()))

    (for ((coor available-floor-coors))
         (for ((maybe (get-nsew-coors coor)))
              (when (hash-has-key? lookup maybe) ; #hash((1 . 1) => '((2 . 1), ...)
                (hash-set! lookup coor (cons maybe (hash-ref lookup coor))))))

    (for ((maybe (get-nsew-coors unit-coor)))
         (when (hash-has-key? lookup maybe)
           (hash-set! lookup unit-coor (cons maybe (hash-ref lookup unit-coor)))))

    (for ((coor-from available-floor-coors))
         (for ((coor-to (hash-ref lookup coor-from)))
              (define edge (list coor-from coor-to))
              (set! vertices (cons edge vertices))))
    (unweighted-graph/undirected vertices)))


(define (choose-nearest-coor unit-coor in-range-coors cave units)
  (let* ((available-floor-coors (get-available-floor-coors cave units))
         (graph-info (convert-to-graph available-floor-coors unit-coor)))
    (define-values (distances ignore) (bfs graph-info unit-coor))
    (define reachable (filter (lambda (coor) (and (member coor in-range-coors)
                                                  (not (equal? (hash-ref distances coor) +inf.0))))
                              (hash-keys distances)))
    (if (null? reachable)
        #f
        (let* ((shortest-distance (apply min (map (lambda (key) (hash-ref distances key)) reachable)))
               (shortest-coors (filter (lambda (key) (= shortest-distance (hash-ref distances key))) reachable))
               (goal-coor (car (sort shortest-coors reading-order)))
               (shortest-path (fewest-vertices-path graph-info unit-coor goal-coor)))
          (cadr shortest-path)))))

(define (move-unit unit-coor cave units)
  (let* ((unit (hash-ref units unit-coor))
         (target-coors (get-target-coors unit units))
         (in-range-coors (get-in-range-coors unit-coor target-coors cave units))
         (move-to-coor (choose-nearest-coor unit-coor in-range-coors cave units)))
    move-to-coor))

(define (part-one filename)
  (define-values (cave units) (load-cave filename))
  (let loop ((rnd 0))
   (draw-cave cave units)
   (for ((unit-coor (sort (hash-keys units) reading-order)))
        (define unit (hash-ref units unit-coor))
        (define enemy (if (eq? (unit-type unit) 'elf) 'goblin 'elf))
        ; Combat?
        (when (> (length (get-units units enemy)) 0)
          (printf "Hi Edward, moving ~a, ~a to ~a~n" unit-coor unit (move-unit unit-coor cave units))
          )
        )

   ;   (when (not (zero? (length (get-units units 'goblin)))) (loop (add1 rnd)))
   ))


