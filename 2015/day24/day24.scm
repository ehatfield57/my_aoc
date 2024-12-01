
(require rackunit)
(require racket/trace)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 24: It Hangs in the Balance ---
;
; It's Christmas Eve, and Santa is loading up the sleigh for this year's deliveries.
;  However, there's one small problem: he can't get the sleigh to balance. If it isn't
;  balanced, he can't defy physics, and nobody gets presents this year.
;
; No pressure.
;
; Santa has provided you a list of the weights of every package he needs to fit on the
;  sleigh. The packages need to be split into three groups of exactly the same weight,
;  and every package has to fit. The first group goes in the passenger compartment of
;  the sleigh, and the second and third go in containers on either side. Only when all
;  three groups weigh exactly the same amount will the sleigh be able to fly. Defying
;  physics has rules, you know!
;
; Of course, that's not the only problem. The first group - the one going in the passenger
;  compartment - needs as few packages as possible so that Santa has some legroom left over.
;  It doesn't matter how many packages are in either of the other two groups, so long as
;  all of the groups weigh the same.
;
; Furthermore, Santa tells you, if there are multiple ways to arrange the packages such
;  that the fewest possible are in the first group, you need to choose the way where the
;  first group has the smallest quantum entanglement to reduce the chance of any "complications".
;  The quantum entanglement of a group of packages is the product of their weights, that is,
;  the value you get when you multiply their weights together. Only consider quantum entanglement
;  if the first group has the fewest possible number of packages in it and all groups weigh the
;  same amount.
;
; For example, suppose you have ten packages with weights 1 through 5 and 7 through 11. For
;  this situation, some of the unique first groups, their quantum entanglements, and a way to
;  divide the remaining packages are as follows:
;
; Group 1;             Group 2; Group 3
; 11 9       (QE= 99); 10 8 2;  7 5 4 3 1
; 10 9 1     (QE= 90); 11 7 2;  8 5 4 3
; 10 8 2     (QE=160); 11 9;    7 5 4 3 1
; 10 7 3     (QE=210); 11 9;    8 5 4 2 1
; 10 5 4 1   (QE=200); 11 9;    8 7 3 2
; 10 5 3 2   (QE=300); 11 9;    8 7 4 1
; 10 4 3 2 1 (QE=240); 11 9;    8 7 5
; 9 8 3      (QE=216); 11 7 2;  10 5 4 1
; 9 7 4      (QE=252); 11 8 1;  10 5 3 2
; 9 5 4 2    (QE=360); 11 8 1;  10 7 3
; 8 7 5      (QE=280); 11 9;    10 4 3 2 1
; 8 5 4 3    (QE=480); 11 9;    10 7 2 1
; 7 5 4 3 1  (QE=420); 11 9;    10 8 2
;
; Of these, although 10 9 1 has the smallest quantum entanglement (90), the configuration with
;  only two packages, 11 9, in the passenger compartment gives Santa the most legroom and wins.
;  In this situation, the quantum entanglement for the ideal configuration is therefore 99. Had
;  there been two configurations with only two packages in the first group, the one with the
;  smaller quantum entanglement would be chosen.
;
; What is the quantum entanglement of the first group of packages in the ideal configuration?
;  (Answer: 11846773891)
;

(define test-data '(11 10 9 8 7 5 4 3 2 1))
(define real-data (sort '(1 2 3 7 11 13 17 19 23 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113) >))
(define solutions (make-hash))

(define (magic goal lst sol tried)
  (let ((sum (apply + sol)))
   (cond
     ((= goal sum)
      (printf "~a ~a ==> Got one: ~a~n" (length sol) (apply * sol) (sort sol >))
      (if (hash-has-key? solutions (sort sol >))
          (hash-set! solutions (sort sol >) (add1 (hash-ref solutions (sort sol >))))
          (hash-set! solutions (sort sol >) 1))
      (cons (reverse sol) (magic goal (sort (append tried lst) >) '() '())))

     ((null? lst)
      (if (null? tried) '() (magic goal (append tried sol) '() '())))

     ((> goal sum)
      (magic goal (cdr lst) (cons (car lst) sol) tried))

     ((< goal sum)
      (magic goal lst (cdr sol) (cons (car sol) tried))))))

(define (poof lst)
  (let ((goal (/ (apply + lst) 3))
        (len (length lst)))
    (let loop ((a-list lst) (cnt 0))
     (magic goal a-list '() '())
     (when (< cnt len)
       (loop (append (cdr a-list) (list (car a-list))) (add1 cnt))))))

; Actual answer supplied by Gemini in Python.  It produced a list of x
;  items which I sorted by QE and list length to get: 11846773891

;; Python code from Gemini:
; import itertools
; 
; def find_combinations_with_sum(data, target_sum):
;     combinations_list = []
;     for r in range(1, len(data) + 1):  # Iterate over combination lengths
;         for combination in itertools.combinations(data, r):
;             if sum(combination) == target_sum:
;                 combinations_list.append(combination)
;     return combinations_list
; 
; data = [1, 2, 3, 4, 5]
; target_sum = 6

; I editited the file to just have the list of numbers and then added a
;  function to the top:
; (define (foo lst) (printf "~a ~a ~a ~a~n" (length lst) (apply + lst) (apply * lst) lst))
; (foo '(1 89 101 107 109 113))
; (foo '(3 97 101 103 107 109))

; Then with this output I sorted via unix using: sort -k 1,1 foo.txt > bar.txt
;  to get the smallest length of list (6) and then: sort -k 3,3 foo.txt > bar.txt
;  to get the smallest EQ (multiplying all the numbers together): 11846773891

;
; --- Part Two ---
; That's weird... the sleigh still isn't balancing.
;
; "Ho ho ho", Santa muses to himself. "I forgot the trunk".
;
; Balance the sleigh again, but this time, separate the packages into four groups
;  instead of three. The other constraints still apply.
;
; Given the example packages above, this would be some of the new unique first
;  groups, their quantum entanglements, and one way to divide the remaining packages:
;
; 11 4    (QE=44); 10 5;   9 3 2 1; 8 7
; 10 5    (QE=50); 11 4;   9 3 2 1; 8 7
; 9 5 1   (QE=45); 11 4;   10 3 2;  8 7
; 9 4 2   (QE=72); 11 3 1; 10 5;    8 7
; 9 3 2 1 (QE=54); 11 4;   10 5;    8 7
; 8 7     (QE=56); 11 4;   10 5;    9 3 2 1
;
; Of these, there are three arrangements that put the minimum (two) number of packages
;  in the first group: 11 4, 10 5, and 8 7. Of these, 11 4 has the lowest quantum
;  entanglement, and so it is selected.
;
; Now, what is the quantum entanglement of the first group of packages in the ideal
;  configuration?  Answer: 80393059
;

; Using the same technique as above, but with this python code, to avoid using vi on a long line

; import itertools
; 
; def find_combinations_with_sum(data, target_sum):
;     for r in range(1, len(data) + 1):
;         for combination in itertools.combinations(data, r):
;             if sum(combination) == target_sum:
;                 print(combination)
; 
; data = [1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113]
; target_sum = 390
; 
; find_combinations_with_sum(data, target_sum)
