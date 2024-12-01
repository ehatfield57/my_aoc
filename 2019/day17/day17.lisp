;;; day17.lisp
;;; Main Program for Advent of Code 2019, Day 17

(defpackage :day17
  (:use :cl :intcode)
  (:export :main))

(in-package :day17)

;;; Define a simple split-string function to avoid external dependencies
(defun split-string (string delimiter)
  "Splits STRING into a list of substrings separated by DELIMITER (a character)."
  (let ((start 0)
        (result '()))
    (loop for i below (length string)
          do (when (char= (char string i) delimiter)
               (push (subseq string start i) result)
               (setf start (1+ i))))
    (push (subseq string start (length string)) result)
    (nreverse result)))

;;; Helper function: Convert a string to a list of ASCII codes, appending a newline character (10)
(defun string-to-ascii-list (str)
  "Converts a string STR to a list of ASCII codes, appending a newline (10)."
  (append (map #'char-code (coerce str 'list)) (list 10)))

;;; Convert ASCII outputs to a 2D grid (vector of vectors)
(defun ascii-to-grid (outputs)
  "Converts a list of ASCII outputs to a 2D grid represented as a vector of vectors."
  (let* ((chars (map #'code-char outputs))
         (combined-str (concatenate 'string chars))
         (lines (remove-if #'(lambda (line) (string= line "")) (split-string combined-str #\Newline))))
    ;; Convert each line into a vector of characters
    (loop for line in lines collect (coerce line 'vector))))

;;; Print the grid to the console
(defun print-grid (grid)
  "Prints the 2D GRID to the console."
  (loop for row across grid do
       (loop for char across row do (format t "~c" char))
       (terpri)))

;;; Check if a given cell is a scaffold ('#')
(defun is-scaffold-p (grid r c)
  "Checks if the cell at row R and column C in GRID is a scaffold ('#')."
  (and (>= r 0) (< r (length grid))
       (>= c 0) (< c (length (aref grid 0)))
       (char= (aref (aref grid r) c) #\#)))

;;; Find intersections and calculate the sum of alignment parameters
(defun find-intersections (grid)
  "Finds scaffold intersections in GRID and returns the sum of their alignment parameters."
  (let ((height (length grid))
        (width (if (> (length grid) 0) (length (aref grid 0)) 0))
        (sum 0))
    (loop for r from 1 below (- height 1)
          do (loop for c from 1 below (- width 1)
                    do (when (and (is-scaffold-p grid r c)
                                  (is-scaffold-p grid (- r 1) c)    ; Up
                                  (is-scaffold-p grid (+ r 1) c)    ; Down
                                  (is-scaffold-p grid r (- c 1))    ; Left
                                  (is-scaffold-p grid r (+ c 1)))   ; Right
                         (incf sum (* r c)))))
    sum))

;;; Function to prepare movement instructions (Part 2)
;;; For demonstration purposes, these functions are hard-coded.
;;; In a complete solution, you'd analyze the scaffold map to determine these.
(defun prepare-movement-instructions ()
  "Prepares the movement instructions for Part 2."
  ;; Define the main routine and movement functions as strings
  ;; These should be determined based on the scaffold map analysis
  ;; Example placeholders:
  (let ((main-routine "A,B,A,B,A,B")
        (function-a "R,8,L,10,R,8")
        (function-b "L,6,R,8,R,8")
        ;; (function-c "R,4,R,4,R,8") ;; Define if needed
        (continuous-video-feed "n"))
    ;; Encode the instructions into ASCII
    (append (string-to-ascii-list main-routine)
            (string-to-ascii-list function-a)
            (string-to-ascii-list function-b)
            ;; (string-to-ascii-list function-c) ;; Encode if defined
            (string-to-ascii-list continuous-video-feed))))

;;; Main function to execute the steps
(defun main ()
  "Main function to execute Day 17 Parts 1 and 2."
  ;; Read the Intcode program from 'day17-data.txt'
  (with-open-file (stream "day17-data.txt" :direction :input)
    (let ((input (read-line stream)))
      (let ((program (intcode:parse-program input))
            (initial-state (intcode:init-intcode (intcode:parse-program input))))
        ;; Run the Intcode program (Part 1)
        (let ((final-state (intcode:run-intcode initial-state)))
          ;; Collect outputs
          (let ((outputs (reverse (intcode:get-outputs final-state)))) ; Reverse to maintain order
            ;; Convert outputs to grid
            (let ((grid (ascii-to-grid outputs)))
              ;; Print the grid
              (format t "Scaffold Map:~%")
              (print-grid grid)
              
              ;; Calculate and print the sum of alignment parameters (Part 1)
              (let ((alignment-sum (find-intersections grid)))
                (format t "Part 1: Sum of alignment parameters = ~A~%" alignment-sum))
              
              ;; --- Part 2 ---
              
              ;; Prepare movement instructions
              ;; In a complete solution, these should be dynamically determined
              ;; based on the scaffold map. Here, they are hard-coded for demonstration.
              (let ((movement-instructions (prepare-movement-instructions)))
                ;; Add movement instructions as inputs to the Intcode state
                (intcode:add-input final-state movement-instructions)
                
                ;; Run the Intcode program with movement instructions (Part 2)
                (let ((final-state-part2 (intcode:run-intcode final-state)))
                  ;; Collect outputs
                  (let ((outputs-part2 (reverse (intcode:get-outputs final-state-part2))))
                    ;; The final output is the amount of dust collected (last output)
                    (let ((dust-amount
                           (if (null outputs-part2)
                               "No output received."
                               (last outputs-part2))))
                      ;; Print the dust amount
                      (format t "Part 2: Dust collected = ~A~%" dust-amount))))))))))))
