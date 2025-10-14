;;; edebug-examples.el --- Examples for learning edebug

;; This file contains example functions to practice using edebug.
;; To use edebug:
;;   1. Position cursor on a function definition (defun)
;;   2. Press SPC d d (or M-x edebug-defun) to "instrument" the function
;;   3. Call the function to start debugging
;;   4. Use SPC d c (continue), SPC d n (next), SPC d i (step in), etc.

;; Load helper functions for cross-file debugging practice
(load-file "~/dotfiles/emacs-playground/helper-functions.el")

;;; Example 1: Simple factorial function
;; Try: Put cursor in defun, press SPC d d, then evaluate (factorial 5)
(defun factorial (n)
  "Calculate factorial of N."
  (if (<= n 1)
      1
    (* n (factorial (- n 1)))))

;;; Example 2: List processing
;; This function has multiple branches and list operations
(defun sum-even-numbers (numbers)
  "Sum only the even numbers in NUMBERS list."
  (let ((sum 0))
    (dolist (num numbers)
      (when (evenp num)
        (setq sum (+ sum num))))
    sum))

;;; Example 3: String manipulation
;; Good for stepping through string operations
(defun reverse-words (sentence)
  "Reverse the order of words in SENTENCE."
  (let* ((words (split-string sentence))
         (reversed (reverse words)))
    (mapconcat 'identity reversed " ")))

;;; Example 4: Nested function calls
;; Practice stepping in and out of function calls
(defun celsius-to-fahrenheit (celsius)
  "Convert CELSIUS to Fahrenheit."
  (+ (* celsius 9/5) 32))

(defun temperature-report (temp-c)
  "Generate a temperature report for TEMP-C in Celsius."
  (let ((temp-f (celsius-to-fahrenheit temp-c)))
    (format "Temperature: %d°C = %.1f°F" temp-c temp-f)))

;;; Example 5: Error-prone function
;; Good for debugging logic errors
(defun find-max (numbers)
  "Find the maximum number in NUMBERS list.
   Note: This has a subtle bug when list contains negative numbers!"
  (let ((max 0))  ; Bug: assumes max starts at 0
    (dolist (num numbers)
      (when (> num max)
        (setq max num)))
    max))

;;; Example 6: Complex conditional logic
(defun categorize-age (age)
  "Categorize a person by AGE."
  (cond
   ((< age 0) "Invalid age")
   ((< age 13) "Child")
   ((< age 20) "Teenager")
   ((< age 65) "Adult")
   (t "Senior")))

;;; Example 7: Cross-file function calls
;; This calls functions defined in helper-functions.el
;; Great for practicing stepping between files!
(defun calculate-power-report (n)
  "Calculate and report power sum for N.
   Calls helper functions in different file."
  (let ((result (power-sum n)))
    (format "For %d: square + cube = %d" n result)))

;;; Example 8: Working with buffers
;; More advanced - interacts with Emacs buffers
(defun count-words-in-buffer ()
  "Count words in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (forward-word 1)
        (setq count (+ count 1)))
      (message "Word count: %d" count)
      count)))

;;; Quick test commands:
;; Evaluate these after instrumenting the functions:
;;
;; (factorial 5)           ; Should return 120
;; (sum-even-numbers '(1 2 3 4 5 6))  ; Should return 12
;; (reverse-words "hello world from emacs")
;; (temperature-report 25)
;; (find-max '(-5 -3 -10 -1))  ; Bug! Returns 0 instead of -1
;; (categorize-age 42)
;; (calculate-power-report 3)  ; Cross-file debugging! Instrument power-sum, square, cube first

;;; Edebug quick reference:
;; SPC d d - Instrument function for debugging
;; SPC d c - Continue (run until next breakpoint)
;; SPC d n - Next (step over)
;; SPC d i - Step in (step into function calls)
;; SPC d o - Step out (finish current function)
;; SPC d b - Set/unset breakpoint
;; SPC d e - Evaluate expression
;; SPC d q - Quit debugging

;;; edebug-examples.el ends here
