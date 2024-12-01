
(require rackunit)
(require srfi/1)

;
; Advent of Code - 2018
;
; Part One
;
; --- Day 4: Repose Record ---
;
; You've sneaked into another supply closet - this time, it's across
;  from the prototype suit manufacturing lab. You need to sneak inside
;  and fix the issues with the suit, but there's a guard stationed
;  outside the lab, so this is as close as you can safely get.
;
; As you search the closet for anything that might help, you discover
;  that you're not the first person to want to sneak in. Covering the
;  walls, someone has spent an hour starting every midnight for the
;  past few months secretly observing this guard post! They've been
;  writing down the ID of the one guard on duty that night - the Elves
;  seem to have decided that one guard was enough for the overnight
;  shift - as well as when they fall asleep or wake up while at their
;  post (your puzzle input).
;
; For example, consider the following records, which have already been
;  organized into chronological order:
;
; [1518-11-01 00:00] Guard #10 begins shift
; [1518-11-01 00:05] falls asleep
; [1518-11-01 00:25] wakes up
; [1518-11-01 00:30] falls asleep
; [1518-11-01 00:55] wakes up
; [1518-11-01 23:58] Guard #99 begins shift
; [1518-11-02 00:40] falls asleep
; [1518-11-02 00:50] wakes up
; [1518-11-03 00:05] Guard #10 begins shift
; [1518-11-03 00:24] falls asleep
; [1518-11-03 00:29] wakes up
; [1518-11-04 00:02] Guard #99 begins shift
; [1518-11-04 00:36] falls asleep
; [1518-11-04 00:46] wakes up
; [1518-11-05 00:03] Guard #99 begins shift
; [1518-11-05 00:45] falls asleep
; [1518-11-05 00:55] wakes up
;
; Timestamps are written using year-month-day hour:minute format. The guard
;  falling asleep or waking up is always the one whose shift most recently
;  started. Because all asleep/awake times are during the midnight hour
;  (00:00 - 00:59), only the minute portion (00 - 59) is relevant for those
;  events.
;
; Visually, these records show that the guards are asleep at these times:
;
; Date   ID   Minute
;             000000000011111111112222222222333333333344444444445555555555
;             012345678901234567890123456789012345678901234567890123456789
; 11-01  #10  .....####################.....#########################.....
; 11-02  #99  ........................................##########..........
; 11-03  #10  ........................#####...............................
; 11-04  #99  ....................................##########..............
; 11-05  #99  .............................................##########.....
;
; The columns are Date, which shows the month-day portion of the relevant day;
;  ID, which shows the guard on duty that day; and Minute, which shows the
;  minutes during which the guard was asleep within the midnight hour. (The
;  Minute column's header shows the minute's ten's digit in the first row and
;  the one's digit in the second row.) Awake is shown as ., and asleep is
;  shown as #.
;
; Note that guards count as asleep on the minute they fall asleep, and they
;  count as awake on the minute they wake up. For example, because Guard #10
;  wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.
;
; If you can figure out the guard most likely to be asleep at a specific time,
;  you might be able to trick that guard into working tonight so you can have
;  the best chance of sneaking in. You have two strategies for choosing the
;  best guard/minute combination.
;
; Strategy 1: Find the guard that has the most minutes asleep. What minute does
;  that guard spend asleep the most?
;
; In the example above, Guard #10 spent the most minutes asleep, a total of 50
;  minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes
;  (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas
;  any other minute the guard was asleep was only seen on one day).
;
; While this example listed the entries in chronological order, your entries are
;  in the order you found them. You'll need to organize them before they can be
;  analyzed.
;
; What is the ID of the guard you chose multiplied by the minute you chose? (In
;  the above example, the answer would be 10 * 24 = 240.) (Answer: 104764)
;

(define (get-raw-data filename) (sort (file->lines filename) string<?))

(define (get-minute line)
  (string->number (caar (regexp-match* #rx":([0-9][0-9])" line #:match-select cdr))))

(define (get-data filename)
  (let ((guard-sleep (make-hash)) ; GuardId -> (10 5 3 ... minutes alseep)
        (sleep-guard (make-hash)) ; minuteId -> (gid gid ... guard ids)
        (raw-data (get-raw-data filename))
        (current-guard-id 0)
        (started-sleeping 0))
    (for ((line raw-data))
         (cond
           ((regexp-match #rx"begins shift" line)
            (set! current-guard-id (string->number (caar (regexp-match* #rx"#([0-9]+)" line #:match-select cdr)))))

           ((regexp-match #rx"falls asleep" line)
            (set! started-sleeping (get-minute line)))

           ((regexp-match #rx"wakes up" line)
            (define mins (- (get-minute line) started-sleeping))
            (if (hash-has-key? guard-sleep current-guard-id)
                (hash-set! guard-sleep current-guard-id (cons mins (hash-ref guard-sleep current-guard-id)))
                (hash-set! guard-sleep current-guard-id (list mins)))
            (for ((m (range started-sleeping (get-minute line))))
                 (if (hash-has-key? sleep-guard m)
                     (hash-set! sleep-guard m (cons current-guard-id (hash-ref sleep-guard m)))
                     (hash-set! sleep-guard m (list current-guard-id))))
            (set! started-sleeping 0))
           (else (error (format "Unknown type of line in raw data: ~a" line)))))
    (values guard-sleep sleep-guard)))

(define (strategy1 filename)
  (define-values (guard-sleep sleep-guard) (get-data filename))

  (define sleepiest-guard (foldl (lambda (key acc)
                                   (let ((gid (car acc))
                                         (val (cdr acc))
                                         (sum (apply + (hash-ref guard-sleep key))))
                                     (if (> val sum)
                                         acc
                                         (cons key sum)))) (cons 0 0) (hash-keys guard-sleep)))

  (define sleepiest-minute (foldl (lambda (m acc)
                                    (let ((occurances (cdr acc))
                                          (minute (car acc))
                                          (guard-seen (length (filter
                                                                (lambda (g) (= (car sleepiest-guard) g))
                                                                (hash-ref sleep-guard m)))))
                                      (if (> guard-seen occurances)
                                          (cons m guard-seen)
                                          acc)))
                                  (cons 0 0) (hash-keys sleep-guard)))
  (* (car sleepiest-guard) (car sleepiest-minute)))

(check-equal? (strategy1 "test-data.txt") 240)
(check-equal? (strategy1 "day04-data.txt") 104764)

;
; --- Part Two ---
;
; Strategy 2: Of all guards, which guard is most frequently asleep on
;  the same minute?
;
; In the example above, Guard #99 spent minute 45 asleep more than any
;  other guard or minute - three times in total. (In all other cases,
;  any guard spent any minute asleep at most twice.)
;
; What is the ID of the guard you chose multiplied by the minute you
;  chose? (In the above example, the answer would be 99 * 45 = 4455.)
;  (Answer: 128617)

(define (count-occurrences value lst)
  (length (filter (lambda (x) (equal? x value)) lst)))

(define (guard-summary guards)
  (let ((uniq-guards (remove-duplicates guards))
        (max-guard-id 0)
        (max-guard-cnt 0))
    (for ((g uniq-guards))
         (let ((guard-cnt (count-occurrences g guards)))
          (when (> guard-cnt max-guard-cnt)
            (set! max-guard-cnt guard-cnt)
            (set! max-guard-id g))))
    (cons max-guard-id max-guard-cnt)))

(define (strategy2 sleep-guard)
  (let ((max-guard-cnt 0)
        (max-guard-id 0)
        (max-minute 0))
    (for ((minute (hash-keys sleep-guard)))
         (define guard-info (guard-summary (hash-ref sleep-guard minute)))
         (when (> (cdr guard-info) max-guard-cnt)
           (set! max-guard-cnt (cdr guard-info))
           (set! max-guard-id (car guard-info))
           (set! max-minute minute)))
    (* max-minute max-guard-id)))

(define (part-two filename)
  (define-values (guard-sleep sleep-guard) (get-data filename))
  (strategy2 sleep-guard))

(check-equal? (part-two "test-data.txt") 4455)
(check-equal? (part-two "day04-data.txt") 128617)

