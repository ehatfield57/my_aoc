#!/usr/bin/env sbcl --script

;;; Advent of Code 2019, Day 18
;;; Solution in Common Lisp (SBCL)

(defpackage :aoc-day18
  (:use :cl)
  (:export :main))

(in-package :aoc-day18)

;;; Structure Definitions

;; Renamed from 'position' to 'pos' to avoid package conflicts
(defstruct pos
  x
  y)

(defstruct state
  pos
  keys
  steps)

;;; Helper Functions

(defun key-bit (key)
  "Returns the bitmask for a given key character."
  (let ((index (- (char-code key) (char-code #\a))))
    (ash 1 index)))

(defun door-p (char)
  "Returns true if the character is a door (uppercase)."
  (and (char>= char #\A) (char<= char #\Z)))

(defun key-p (char)
  "Returns true if the character is a key (lowercase)."
  (and (>= char #\a) (<= char #\z)))

(defun char-downcase (char)
  "Converts a character to its lowercase equivalent."
  (if (and (>= char #\A) (<= char #\Z))
      (code-char (+ (- (char-code char) (char-code #\A))
                    (char-code #\a)))
      char))

;;; Queue Implementation using Two Lists

(defun make-queue ()
  "Creates an empty queue."
  (list '() '()))

(defun enqueue (queue item)
  "Enqueues an item to the queue."
  (let ((in (first queue)))
    (setf (first queue) (cons item in))))

(defun dequeue (queue)
  "Dequeues an item from the queue."
  (let ((in (first queue))
        (out (second queue)))
    (when (null out)
      (setf out (reverse in))
      (setf (second queue) out)
      (setf (first queue) '()))
    (let ((item (first out)))
      (setf (second queue) (rest out))
      item)))

(defun queue-empty-p (queue)
  "Checks if the queue is empty."
  (and (null (first queue)) (null (second queue))))

;;; Maze Reading and Parsing

(defun read-maze ()
  "Reads the maze from standard input and returns a 2D array representing the maze."
  (let ((maze-lines (loop for line = (read-line *standard-input* nil)
                           while line
                           collect (coerce line 'vector)))
        (height 0)
        (width 0))
    (setq height (length maze-lines))
    (setq width (if height (length (first maze-lines)) 0))
    (let ((maze (make-array (list height width) :element-type 'character :initial-element #\#)))
      (loop for y below height
            do (loop for x below width
                     for char = (if (< x (length (nth y maze-lines)))
                                   (elt (nth y maze-lines) x)
                                   #\#)
                     do (setf (aref maze y x) char)))
      maze)))

(defun find-start-and-keys (maze width height)
  "Finds the starting position and computes the total key mask."
  (let ((start-pos nil)
        (total-key-mask 0))
    (loop for y below height
          do (loop for x below width
                   for char = (aref maze y x)
                   do (cond
                        ((char= char #\@)
                         (setf start-pos (make-pos :x x :y y)))
                        ((key-p char)
                         (setf total-key-mask (logior total-key-mask (key-bit char))))))
          )
    (values start-pos total-key-mask)))

;;; BFS Algorithm to Collect All Keys

(defun enqueue-state (queue pos keys steps visited)
  "Enqueues a new state if it hasn't been visited with the current key set."
  (let ((key (list (pos-x pos) (pos-y pos) keys)))
    (unless (gethash key visited)
      (setf (gethash key visited) t)
      (enqueue queue (make-state :pos pos :keys keys :steps steps)))))

(defun adjacent-positions (pos maze width height)
  "Returns a list of adjacent positions that are not walls."
  (let ((directions '((0 1) (1 0) (0 -1) (-1 0)))
        (adjacents '()))
    (dolist (dir directions adjacents)
      (let* ((new-x (+ (pos-x pos) (first dir)))
             (new-y (+ (pos-y pos) (second dir))))
        (when (and (>= new-x 0) (< new-x width)
                   (>= new-y 0) (< new-y height))
          (let ((char (aref maze new-y new-x)))
            (unless (char= char #\#)
              (push (make-pos :x new-x :y new-y) adjacents))))))
    adjacents))

(defun bfs (maze width height start-pos total-key-mask)
  "Performs BFS to find the minimum number of steps to collect all keys."
  (let* ((queue (make-queue))
         (visited (make-hash-table :test 'equal))
         (initial-state (make-state :pos start-pos :keys 0 :steps 0)))
    (enqueue queue initial-state)
    (setf (gethash (list (pos-x start-pos) (pos-y start-pos) 0) visited) t)
    (loop while (not (queue-empty-p queue))
          for current = (dequeue queue)
          do (let ((current-pos (state-pos current))
                   (current-keys (state-keys current))
                   (current-steps (state-steps current)))
               ;; Check if all keys collected
               (when (= current-keys total-key-mask)
                 (return current-steps))
               ;; Explore adjacent positions
               (dolist (adj (adjacent-positions current-pos maze width height))
                 (let ((char (aref maze (pos-y adj) (pos-x adj)))
                       (new-keys current-keys))
                   (cond
                     ;; Door, check if key is collected
                     (format T "Hi Edward A, char:~a%" char)
                     ((door-p char)
                      (let ((required-key (char-downcase char)))
                        (when (logand current-keys (key-bit required-key))
                          (enqueue-state queue adj current-keys (1+ current-steps) visited))))
                     ;; Key, collect it
                     ((key-p char)
                      (let ((new-key-bit (key-bit char))
                            (updated-keys (logior current-keys (key-bit char))))
                        (enqueue-state queue adj updated-keys (1+ current-steps) visited)))
                     ;; Open passage
                     (t
                      (enqueue-state queue adj current-keys (1+ current-steps) visited))))))))
    ;; If all keys are not reachable
    nil)

;;; Main Function

(defun main ()
  "Main function to solve AoC 2019 Day 18."
  (let* ((maze (read-maze))
         (height (array-dimension maze 0))
         (width (array-dimension maze 1))
         (start-and-keys (find-start-and-keys maze width height))
         (start-pos (first start-and-keys))
         (total-key-mask (second start-and-keys))
         (result (bfs maze width height start-pos total-key-mask)))
    (if result
        (format t "Minimum number of steps to collect all keys: ~a~%" result)
        (format t "Unable to collect all keys.~%"))))

;;; Execute the Main Function
(main)
