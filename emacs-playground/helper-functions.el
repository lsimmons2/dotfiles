;;; helper-functions.el --- Helper functions called by edebug-examples.el

;; This file contains helper functions that are called from edebug-examples.el
;; Use this to practice stepping between different files/buffers in edebug.

;;; Helper functions for mathematical operations

(defun square (n)
  "Return the square of N."
  (* n n))

(defun cube (n)
  "Return the cube of N."
  (* n n n))

(defun power-sum (n)
  "Return the sum of N squared plus N cubed.
   Calls functions in different file to demonstrate cross-file debugging."
  (+ (square n) (cube n)))

;;; Helper functions for list operations

(defun double-value (x)
  "Double the value of X."
  (* 2 x))

(defun process-list (numbers)
  "Double each number in NUMBERS list."
  (mapcar 'double-value numbers))

;;; Helper functions for string operations

(defun add-prefix (prefix str)
  "Add PREFIX to STR."
  (concat prefix str))

(defun make-greeting (name)
  "Create a greeting for NAME."
  (add-prefix "Hello, " name))

;;; Test these functions:
;; First instrument them with SPC d d, then call from edebug-examples.el
;; or evaluate directly:
;;
;; (power-sum 3)        ; Calls square and cube
;; (process-list '(1 2 3 4))  ; Calls double-value for each element
;; (make-greeting "World")    ; Calls add-prefix

;;; helper-functions.el ends here
