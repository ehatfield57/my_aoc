
(require rackunit)

;
; Advent of Code - 2015
;
; Part One
;
; --- Day 19: Medicine for Rudolph ---
;
; Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly,
;  and he needs medicine.
;
; Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph
;  is going to need custom-made medicine. Unfortunately, Red-Nosed Reindeer
;  chemistry isn't similar to regular reindeer chemistry, either.
;
; The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission
;  plant, capable of constructing any Red-Nosed Reindeer molecule you need. It
;  works by starting with some input molecule and then doing a series of
;  replacements, one per step, until it has the right molecule.
;
; However, the machine has to be calibrated before it can be used. Calibration
;  involves determining the number of molecules that can be generated in one
;  step from a given starting point.
;
; For example, imagine a simpler machine that supports only the following
;  replacements:
;
; H => HO
; H => OH
; O => HH
;
; Given the replacements above and starting with HOH, the following molecules
;  could be generated:
;
; * HOOH (via H => HO on the first H).
; * HOHO (via H => HO on the second H).
; * OHOH (via H => OH on the first H).
; * HOOH (via H => OH on the second H).
; * HHHH (via O => HH).
;
; So, in the example above, there are 4 distinct molecules (not five, because
;  HOOH appears twice) after one replacement from HOH. Santa's favorite molecule,
;  HOHOHO, can become 7 distinct molecules (over nine replacements: six from H,
;  and three from O).
;
; The machine replaces without regard for the surrounding characters. For example,
;  given the string H2O, the transition H => OO would result in OO2O.
;
; Your puzzle input describes all of the possible replacements and, at the bottom,
;  the medicine molecule for which you need to calibrate the machine. How many
;  distinct molecules can be created after all the different ways you can do one
;  replacement on the medicine molecule?  (Answer: 576)
;

(define test-replacements '(("H" "HO") ("H" "OH") ("O" "HH")))
(define test-molecule "HOH")
(define santa-molecule "HOHOHO")

(define (apply-replacements molecule replacements)
  (let ((results (make-hash)))
   (for/list ((replacement replacements))
             (let ((m-find (car replacement))
                   (m-replace (cadr replacement)))
               (let* ((m-regexp (regexp m-find))
                      (positions (regexp-match-positions* m-regexp molecule)))
                 (for/list ((position positions))
                           (let ((start-pos (car position))
                                 (end-pos (cdr position)))
                             (let* ((pre-match (substring molecule 0 start-pos))
                                    (post-match (substring molecule end-pos (string-length molecule)))
                                    (out (string-append pre-match m-replace post-match)))
                               (if (hash-has-key? results out)
                                   (hash-set! results out (add1 (hash-ref results out)))
                                   (hash-set! results out 1))
                               ))))))
   results))

(define (count-distinct molecule replacements)
  (length (hash-keys (apply-replacements molecule replacements))))

(check-equal? (count-distinct test-molecule test-replacements) 4)
(check-equal? (count-distinct santa-molecule test-replacements) 7)

(define (get-data filename)
  (let ((replacements empty)
        (molecule ""))
    (call-with-input-file
      filename
      (lambda (in-port)
        (do ((line (read-line in-port 'any)
                   (read-line in-port 'any)))
            ((eof-object? line))
            (let ((matches (regexp-match #rx"^([^ ]+) => (.*)$" line)))
              (cond
                ((string=? line ""))
                (matches (set! replacements (cons (list (second matches) (third matches)) replacements)))
                (else (set! molecule line)))))))
    (values molecule (reverse replacements))))

(define-values (molecule replacements) (get-data "day19-data.txt"))

(check-equal? (count-distinct molecule replacements) 576)

;
; --- Part Two ---
;
; Now that the machine is calibrated, you're ready to begin molecule fabrication.
; 
; Molecule fabrication always begins with just a single electron, e, and applying
;  replacements one at a time, just like the ones during calibration.
; 
; For example, suppose you have the following replacements:
; 
; e => H
; e => O
; H => HO
; H => OH
; O => HH
;
; If you'd like to make HOH, you start with e, and then make the following replacements:
; 
; * e => O to get O
; * O => HH to get HH
; * H => OH (on the second H) to get HOH
;
; So, you could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can be made in 6 steps.
;
; How long will it take to make the medicine? Given the available replacements and the medicine
;  molecule in your puzzle input, what is the fewest number of steps to go from e to the medicine
;  molecule? (Answer: 207)
;
; --------------------------------------------------------------------------------
;
; ==> Solution via reddit from askalski (Not a computer coded solution)
;
; No leaderboard for me today, because I decided to sleep on Part 2, before
;  solving it by hand. Since there's really no code to speak of, I'll talk
;  about the solution.
;
; First insight
; =============
; There are only two types of productions:
;
; e => XX and X => XX (X is not Rn, Y, or Ar)
;
; X => X Rn X Ar | X Rn X Y X Ar | X Rn X Y X Y X Ar
;
; Second insight
; ==============
; You can think of Rn Y Ar as the characters ( , ):
;
; X => X(X) | X(X,X) | X(X,X,X)
;
; Whenever there are two adjacent "elements" in your "molecule", you apply
;  the first production. This reduces your molecule length by 1 each time.
;
; And whenever you have T(T) T(T,T) or T(T,T,T) (T is a literal token such
;  as "Mg", i.e. not a nonterminal like "TiTiCaCa"), you apply the second
;  production. This reduces your molecule length by 3, 5, or 7.
;
; Third insight
; =============
; Repeatedly applying X => XX until you arrive at a single token takes
;  count(tokens) - 1 steps:
;
; ABCDE => XCDE => XDE => XE => X
; count("ABCDE") = 5
; 5 - 1 = 4 steps
;
; Applying X => X(X) is similar to X => XX, except you get the () for free.
;  This can be expressed as count(tokens) - count("(" or ")") - 1.
;
; A(B(C(D(E)))) => A(B(C(X))) => A(B(X)) => A(X) => X
; count("A(B(C(D(E))))") = 13
; count("(((())))") = 8
; 13 - 8 - 1 = 4 steps
;
; You can generalize to X => X(X,X) by noting that each , reduces the length by two (,X).
;  The new formula is count(tokens) - count("(" or ")") - 2*count(",") - 1.
;
; A(B(C,D),E(F,G)) => A(B(C,D),X) => A(X,X) => X
; count("A(B(C,D),E(F,G))") = 16
; count("(()())") = 6
; count(",,,") = 3
; 16 - 6 - 2*3 - 1 = 3 steps
;
; This final formula works for all of the production types (for X => XX, the
;  (,) counts are zero by definition.)
;
; The solution
; ============
; My input file had:
;
; 295 elements in total
;  68 were Rn and Ar (the `(` and `)`)
;   7 were Y (the `,`)
;
; Plugging in the numbers:
;
; 295 - 68 - 2*7 - 1 = 212
;
; Like I said, no leaderboard position today, but this was a heck of a lot more
;  interesting than writing yet another brute force script.
;
; --------------------------------------------------------------------------------
;
; ==> topaz2078 (The creator of Advent of Code) replied:
;
; This is the correct answer. In fact, internally, Rn Y Ar exactly map to ( , )!
;
; Actually, there's even more of a pattern, but if you've already gotten this far,
;  I'll wait to explain the rest in case someone wants to try to figure it out on
;  their own. Hint: it's based in chemistry, but not the atoms you're seeing.
;
; You have enough of the pieces here to deduce why the puzzle isn't just "guess
;  randomly and win" - this is actually the production rules for an unambiguous
;  grammar (at least, unambiguous in terms of step count, which is all that matters).
;  My request for the "fewest number of steps" is a decoy - there is only one number
;  of steps possible, as also demonstrated by u/askalski's equation.
;
; --------------------------------------------------------------------------------
;
;  Back to me:
;
;  292 elements in total (took apart the molecule)
;   72 were Rn and Ar (the '(' and ')')
;    6 were Y (the ',')
;
; (+ 292 -36 -36 -12 -1) => 207  (Yep, right answer, but I have no idea why)
;

