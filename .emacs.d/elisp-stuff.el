;; Clear Edebug's default keybindings to restore Evil functionality
(setq edebug-mode-map (make-sparse-keymap))
;; Add Evil integration hook
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

;; Bindings for editing Emacs Lisp files
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "SPC d d") 'edebug-defun                    ; instrument function
  )

;; Bindings for when you're actively debugging
(evil-define-key 'normal edebug-mode-map
  (kbd "SPC d e") 'edebug-eval-expression    ; evaluate expression
  (kbd "SPC d c") 'edebug-go-mode           ; continue
  (kbd "SPC d n") 'edebug-step-mode          ; next/step
  (kbd "SPC d i") 'edebug-step-in            ; step into
  (kbd "SPC d o") 'edebug-step-out           ; step out
  (kbd "SPC d b") 'edebug-set-breakpoint     ; toggle breakpoint
  (kbd "SPC d r") 'edebug-stop               ; stop/restart
  (kbd "SPC d l") 'edebug-bounce-point       ; show current location
  (kbd "SPC d s") 'edebug-where              ; show status
  (kbd "SPC d p") 'edebug-view-outside       ; view outside windows
  (kbd "SPC d w") 'edebug-where              ; where am I
  (kbd "SPC d d") 'edebug-pop-to-backtrace
  (kbd "SPC d q") 'edebug-top-level-nonstop  ; quit debugging
  )

;; Optional: Configure Edebug behavior
;; (setq edebug-all-defs nil)           ; Don't instrument everything by default
;; (setq edebug-all-forms nil)          ; Don't instrument all forms
;; (setq edebug-save-windows t)         ; Save window configuration

;; Function to list all instrumented functions
(defun edebug-list-instrumented-functions ()
  "Display a list of all functions currently instrumented for edebug."
  (interactive)
  ;; Create an empty list to collect instrumented function symbols
  (let ((instrumented-functions '()))
    ;; mapatoms iterates over all symbols in the Emacs obarray (symbol table)
    ;; We pass it a lambda function that will be called for each symbol
    (mapatoms
     (lambda (sym)
       ;; For each symbol, check two things:
       ;; 1. (fboundp sym) - Is this symbol bound to a function?
       ;; 2. (get sym 'edebug) - Does this symbol have an 'edebug property?
       ;;    When edebug instruments a function, it sets this property on the symbol
       (when (and (fboundp sym)
                  (get sym 'edebug))
         ;; If both conditions are true, add this symbol to our list
         (push sym instrumented-functions))))
    ;; Now display the results
    (if instrumented-functions
        ;; If we found instrumented functions, format and display them
        (message "Instrumented functions: %s"
                 ;; mapconcat joins list elements into a string with a separator
                 ;; 'symbol-name converts each symbol to its string name
                 (mapconcat 'symbol-name
                            ;; First sort the list alphabetically for easier reading
                            (sort instrumented-functions
                                  (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                            ", "))  ; Join with ", " separator
      ;; If no instrumented functions found, say so
      (message "No functions are currently instrumented"))))

;; Keybinding to list instrumented functions
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "SPC d l") 'edebug-list-instrumented-functions)
