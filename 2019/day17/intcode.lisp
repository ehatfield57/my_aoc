;;; intcode.lisp
;;; Intcode Interpreter Module for Advent of Code 2019

(defpackage :intcode
  (:use :cl)
  (:export :parse-program
           :init-intcode
           :run-intcode
           :add-input
           :get-outputs))

(in-package :intcode)

;;; Define the IntcodeState structure
(defstruct intcode-state
  memory         ; Hash table mapping addresses to values
  pointer        ; Instruction pointer
  relative-base  ; Relative base for relative mode
  inputs         ; Queue of input values
  outputs        ; List of output values
  halted?)       ; Boolean flag indicating if the program has halted

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

;;; Parse the Intcode program from a string into a mutable hash table memory
(defun parse-program (input)
  "Parses a comma-separated string of integers into a hash table representing memory."
  (let* ((codes (map 'list #'parse-integer (split-string input #\,))) ; Use #\, for comma character
         (memory (make-hash-table :test 'eql)))
    ;; Populate the hash table with the Intcode program
    (loop for i from 0
          for code in codes
          do (setf (gethash i memory) code))
    memory))

;;; Initialize the Intcode state with memory, pointer, relative-base, inputs, outputs, and halted flag
(defun init-intcode (memory)
  "Initializes the IntcodeState with given memory and default values."
  (make-intcode-state
   :memory memory
   :pointer 0
   :relative-base 0
   :inputs '()
   :outputs '()
   :halted? nil))

;;; Add an input value to the Intcode state
(defun add-input (state input)
  "Adds INPUT to the IntcodeState's input queue."
  (setf (intcode-state-inputs state) (append (intcode-state-inputs state) (list input)))
  state)

;;; Retrieve the outputs from the Intcode state
(defun get-outputs (state)
  "Retrieves the list of outputs from the IntcodeState."
  (intcode-state-outputs state))

;;; Get a value from memory with default 0 if the address is not present
(defun get-memory (memory addr)
  "Retrieves the value at address ADDR from MEMORY, defaulting to 0 if not present."
  (or (gethash addr memory) 0))

;;; Set a value in memory based on parameter mode, modifying memory in place
(defun set-param (memory param mode relative-base val)
  "Sets VAL in MEMORY based on MODE at PARAM considering RELATIVE-BASE."
  (cond ((= mode 0) ; Position mode
         (setf (gethash param memory) val))
        ((= mode 2) ; Relative mode
         (setf (gethash (+ relative-base param) memory) val))
        (t
         (error "Unknown parameter mode for writing: ~A" mode))))

;;; Get a parameter value based on its mode
(defun get-param (memory param mode relative-base)
  "Retrieves the parameter value from MEMORY based on MODE and RELATIVE-BASE."
  (cond ((= mode 0) ; Position mode
         (get-memory memory param))
        ((= mode 1) ; Immediate mode
         param)
        ((= mode 2) ; Relative mode
         (get-memory memory (+ relative-base param)))
        (t
         (error "Unknown parameter mode: ~A" mode))))

;;; Run the Intcode program recursively
(defun run-intcode (state)
  "Executes the Intcode program until it halts or waits for input."
  (loop
    while (not (intcode-state-halted? state))
    do (let* ((memory (intcode-state-memory state))
              (pointer (intcode-state-pointer state))
              (relative-base (intcode-state-relative-base state))
              (instr (get-memory memory pointer))
              (opcode (mod instr 100))
              (m1 (floor (/ (mod instr 1000) 100)))
              (m2 (floor (/ (mod instr 10000) 1000)))
              (m3 (floor (/ (mod instr 100000) 10000)))
              (param1 (get-memory memory (+ pointer 1)))
              (param2 (get-memory memory (+ pointer 2)))
              (param3 (get-memory memory (+ pointer 3))))
      
      (cond
        ;; Opcode 1: Add
        ((= opcode 1)
         (let ((val1 (get-param memory param1 m1 relative-base))
               (val2 (get-param memory param2 m2 relative-base)))
           (set-param memory param3 m3 relative-base (+ val1 val2))
           (setf (intcode-state-pointer state) (+ pointer 4))))
        
        ;; Opcode 2: Multiply
        ((= opcode 2)
         (let ((val1 (get-param memory param1 m1 relative-base))
               (val2 (get-param memory param2 m2 relative-base)))
           (set-param memory param3 m3 relative-base (* val1 val2))
           (setf (intcode-state-pointer state) (+ pointer 4))))
        
        ;; Opcode 3: Input
        ((= opcode 3)
         (if (null (intcode-state-inputs state))
             ;; If no input is available, wait for input
             (return)
             ;; Otherwise, consume the first input and continue
             (let ((input (first (intcode-state-inputs state))))
               (set-param memory param1 m1 relative-base input)
               (setf (intcode-state-inputs state) (rest (intcode-state-inputs state)))
               (setf (intcode-state-pointer state) (+ pointer 2)))))
        
        ;; Opcode 4: Output
        ((= opcode 4)
         (let ((val (get-param memory param1 m1 relative-base)))
           (setf (intcode-state-outputs state) (cons val (intcode-state-outputs state)))
           (setf (intcode-state-pointer state) (+ pointer 2))))
        
        ;; Opcode 5: Jump-if-true
        ((= opcode 5)
         (let ((val (get-param memory param1 m1 relative-base))
               (target (get-param memory param2 m2 relative-base)))
           (if (/= val 0)
               (setf (intcode-state-pointer state) target)
               (setf (intcode-state-pointer state) (+ pointer 3)))))
        
        ;; Opcode 6: Jump-if-false
        ((= opcode 6)
         (let ((val (get-param memory param1 m1 relative-base))
               (target (get-param memory param2 m2 relative-base)))
           (if (= val 0)
               (setf (intcode-state-pointer state) target)
               (setf (intcode-state-pointer state) (+ pointer 3)))))
        
        ;; Opcode 7: Less than
        ((= opcode 7)
         (let* ((val1 (get-param memory param1 m1 relative-base))
               (val2 (get-param memory param2 m2 relative-base))
               (result (if (< val1 val2) 1 0)))
           (set-param memory param3 m3 relative-base result)
           (setf (intcode-state-pointer state) (+ pointer 4))))
        
        ;; Opcode 8: Equals
        ((= opcode 8)
         (let* ((val1 (get-param memory param1 m1 relative-base))
               (val2 (get-param memory param2 m2 relative-base))
               (result (if (= val1 val2) 1 0)))
           (set-param memory param3 m3 relative-base result)
           (setf (intcode-state-pointer state) (+ pointer 4))))
        
        ;; Opcode 9: Adjust relative base
        ((= opcode 9)
         (let ((val (get-param memory param1 m1 relative-base)))
           (setf (intcode-state-relative-base state) (+ relative-base val))
           (setf (intcode-state-pointer state) (+ pointer 2))))
        
        ;; Opcode 99: Halt
        ((= opcode 99)
         (setf (intcode-state-halted? state) t))
        
        ;; Unknown Opcode
        (t
         (error "Unknown opcode: ~A" opcode))))))
