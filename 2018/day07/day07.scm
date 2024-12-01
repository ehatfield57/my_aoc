
(require racket/set)
(require rackunit)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 7: The Sum of Its Parts ---
;
; You find yourself standing on a snow-covered coastline; apparently, you
;  landed a little off course. The region is too hilly to see the North
;  Pole from here, but you do spot some Elves that seem to be trying to
;  unpack something that washed ashore. It's quite cold out, so you decide
;  to risk creating a paradox by asking them for directions.
;
; "Oh, are you the search party?" Somehow, you can understand whatever Elves
;  from the year 1018 speak; you assume it's Ancient Nordic Elvish. Could
;  the device on your wrist also be a translator? "Those clothes don't look
;  very warm; take this." They hand you a heavy coat.
;
; "We do need to find our way back to the North Pole, but we have higher
;  priorities at the moment. You see, believe it or not, this box contains
;  something that will solve all of Santa's transportation problems - at
;  least, that's what it looks like from the pictures in the instructions."
;  It doesn't seem like they can read whatever language it's in, but you
;  can: "Sleigh kit. Some assembly required."
;
; "'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh'
;  at once!" They start excitedly pulling more parts out of the box.
;
; The instructions specify a series of steps and requirements about which
;  steps must be finished before others can begin (your puzzle input). Each
;  step is designated by a single letter. For example, suppose you have the
;  following instructions:
;
; Step C must be finished before step A can begin.
; Step C must be finished before step F can begin.
; Step A must be finished before step B can begin.
; Step A must be finished before step D can begin.
; Step B must be finished before step E can begin.
; Step D must be finished before step E can begin.
; Step F must be finished before step E can begin.
;
; Visually, these requirements look like this:
;
;   -->A--->B--
;  /    \      \
; C      -->D----->E
;  \           /
;   ---->F-----
;
; Your first goal is to determine the order in which the steps should be completed.
;  If more than one step is ready, choose the step which is first alphabetically.
;  In this example, the steps would be completed as follows:
;
; * Only C is available, and so it is done first.
; * Next, both A and F are available. A is first alphabetically, so it is done next.
; * Then, even though F was available earlier, steps B and D are now also available,
;    and B is the first alphabetically of the three.
; * After that, only D and F are available. E is not available because only some of
;    its prerequisites are complete. Therefore, D is completed next.
; * F is the only choice, so it is done next.
; * Finally, E is completed.
;
; So, in this example, the correct order is CABDFE.
;
; In what order should the steps in your instructions be completed?
;  (Answer: "FHICMRTXYDBOAJNPWQGVZUEKLS")
;

(define (get-rules filename)
  (let ((rules (make-hash))
        (lines (file->lines filename)))
    (for ((line lines))
         (let* ((pair (flatten (regexp-match* #rx" ([A-Z]) " line #:match-select cdr)))
                 (a (car pair)) (b (cadr pair)))
           (if (hash-has-key? rules a)
               (hash-set! rules a (cons b (hash-ref rules a)))
               (hash-set! rules a (list b)))))
    rules))

(define (find-heads rules)
  (let* ((heads (apply set (sort (hash-keys rules) string<?)))
         (tails (apply set (sort (remove-duplicates (flatten (hash-values rules))) string<?)))
         (diff (set-subtract heads tails)))
    (sort (set->list diff) string<?)))

(define (traverse! rules)
  (let ((heads (find-heads rules)))
   (cond
     ((null? (hash-keys rules)) '())
     ((= 1 (length (hash-keys rules)))
      (let* ((a (car heads))
             (b (hash-ref rules a)))
        (hash-remove! rules a)
        (append (list a) b (traverse! rules))))
     (else
       (let* ((a (car heads)))
        (hash-remove! rules a)
        (append (list a) (traverse! rules)))))))

(define (part-one filename)
  (let ((rules (get-rules filename)))
   (string-join (traverse! rules) "")))

;
; --- Part Two ---
;
; As you're about to begin construction, four of the Elves offer to help.
;  "The sun will set soon; it'll go faster if we work together." Now, you
;  need to account for multiple people working on steps simultaneously. If
;  multiple steps are available, workers should still begin them in alphabetical
;  order.
;
; Each step takes 60 seconds plus an amount corresponding to its letter: A=1,
;  B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step Z takes
;  60+26=86 seconds. No time is required between steps.
;
; To simplify things for the example, however, suppose you only have help from
;  one Elf (a total of two workers) and that each step takes 60 fewer seconds
;  (so that step A takes 1 second and step Z takes 26 seconds). Then, using the
;  same instructions as above, this is how each second would be spent:
;
; Second   Worker 1   Worker 2   Done
;    0        C          .        
;    1        C          .        
;    2        C          .        
;    3        A          F       C
;    4        B          F       CA
;    5        B          F       CA
;    6        D          F       CAB
;    7        D          F       CAB
;    8        D          F       CAB
;    9        D          .       CABF
;   10        E          .       CABFD
;   11        E          .       CABFD
;   12        E          .       CABFD
;   13        E          .       CABFD
;   14        E          .       CABFD
;   15        .          .       CABFDE
;
; Each row represents one second of time. The Second column identifies how many
;  seconds have passed as of the beginning of that second. Each worker column shows
;  the step that worker is currently doing (or . if they are idle). The Done column
;  shows completed steps.
;
; Note that the order of the steps has changed; this is because steps now take time
;  to finish and multiple workers can begin multiple steps simultaneously.
;
; In this example, it would take 15 seconds for two workers to complete these steps.
;
; With 5 workers and the 60+ second step durations described above, how long will it
;  take to complete all of the steps? (Answer: 946)
;

(define (sorted-queue)
  (let ((queue '()))
   (lambda (cmd . args)
     (case cmd
       ((dump) queue)
       ((push!) (set! queue (sort (cons (car args) queue) string<?)))
       ((empty?) (if (zero? (length queue)) #t #f))
       ((remove!) (set! queue (remove (car args) queue)))
       ((pop!)
        (if (zero? (length queue))
            #f
            (let ((return (car queue)))
             (set! queue (cdr queue))
             return)))
       (else (printf "Unknown commands: '~a'~n" cmd))))))

(define (part-two filename worker-cnt sec-offset)
  (let* ((rules (get-rules filename))
         (requirements (get-requirements filename))
         (DEBUG #f)
         (workers '())
         (done '())
         (w-queue (sorted-queue))
         (letter->steps (lambda (letter) (+ sec-offset (- (char->integer (car (string->list letter))) 64))))
         (worker-min (lambda () (sort workers (lambda (a b) (< (cdr a) (cdr b))))))
         (elements (lambda (lst) (map car lst)))
         (workers-avail? (lambda () (< (length workers) worker-cnt)))
         (processed (lambda (s) (if (or (member s (elements workers))
                                        (member s (w-queue 'dump))
                                        (member s (elements done))) #t #f))))

    (for ((h (sort (find-heads rules) string<?)))
         (w-queue 'push! h))

    (let loop ((seconds 0))
     (for ((w workers))
          (when (<= (cdr w) seconds)
            (set! done (cons w done))
            (set! workers (remove w workers))
            (when (hash-has-key? rules (car w))
              (for ((h (hash-ref rules (car w))))
                   (when (not (processed h))
                     (w-queue 'push! h))))))

     (when (and (not (w-queue 'empty?)) (workers-avail?))
       (for ((h (w-queue 'dump)))
            (when (workers-avail?)
              (when (or (not (hash-has-key? requirements h)) (requirements-done requirements done h))
                (set! workers (cons (cons h (+ seconds (letter->steps h))) workers)) 
                (w-queue 'remove! h)))))

     (when DEBUG
       (printf "~nHi Edward A:~n")
       (printf "Seconds: ~a~n" seconds)
       (printf "Rules: ~a~n" rules)
       (printf "Queue: ~a~n" (w-queue 'dump))
       (printf "Workers: ~a~n" workers)
       (printf "worker-min: ~a~n" (worker-min))
       (printf "done: ~a~n" done))

     (if (and (w-queue 'empty?) (null? workers))
         (begin
           (printf "Seconds: ~a, Done:~a~n" seconds (string-join (map car (reverse done)) ""))
           seconds)
         (loop (add1 seconds))))))

;(check-equal? (part-two "test-data.txt" 2 0) 15)
; 449 is too low
; 450 is too low
;(check-equal? (part-two "day07-data.txt" 5 60) 946)

; 'cat day07-data.txt | python day07.py' gives the correct answer of 946

; Ok, I think the reason that my numbers are too low is because I'm not
;  waiting for all the prerequisite steps to be done before doing the
;  next step.  So in the Test Data, I can't start 'E' until 'B,D,F' are
;  done.

(define (get-requirements filename)
  (let ((requirements (make-hash))
        (lines (file->lines filename)))
    (for ((line lines))
         (let* ((pair (flatten (regexp-match* #rx" ([A-Z]) " line #:match-select cdr)))
                 (a (car pair)) (b (cadr pair)))
           (if (hash-has-key? requirements b)
               (hash-set! requirements b (cons a (hash-ref requirements b)))
               (hash-set! requirements b (list a)))))
    requirements))

(define (requirements-done requirements done letter)
  (let ((ok #t))
   (if (hash-has-key? requirements letter)
     (for ((r (hash-ref requirements letter)))
          (when (not (member r (map car done)))
            (set! ok #f)))
     (set! ok #f))
   ok))

(check-equal? (part-two "test-data.txt" 2 0) 15)
(check-equal? (part-two "day07-data.txt" 5 60) 946)

