;; Clear Edebug's default keybindings to restore Evil functionality
(setq edebug-mode-map (make-sparse-keymap))
;; Add Evil integration hook
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

;; Bindings for editing Emacs Lisp files
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "SPC d d") 'edebug-defun                    ; instrument function
  )

;; Bindings for when you're actively debugging
;; All standard Edebug keys mapped to SPC d <key>
(evil-define-key 'normal edebug-mode-map
  ;; Core stepping commands
  (kbd "SPC d SPC") 'edebug-step-mode              ; step (the main stepping key)
  (kbd "SPC d s") 'edebug-step-mode              ; step (the main stepping key)
  (kbd "SPC d n") 'edebug-next-mode                ; next (step over function calls)
  (kbd "SPC d i") 'edebug-step-in                  ; step into (force entry to function)
  (kbd "SPC d o") 'edebug-step-out                 ; step out (finish current function)
  (kbd "SPC d g") 'edebug-go-mode                  ; go (continue until breakpoint)
  (kbd "SPC d c") 'edebug-continue-mode            ; continue with brief pauses
  (kbd "SPC d C") 'edebug-Continue-fast-mode       ; continue without pauses
  (kbd "SPC d G") 'edebug-Go-nonstop-mode          ; go nonstop (ignore breakpoints)
  (kbd "SPC d t") 'edebug-Trace-fast-mode          ; trace without stopping
  (kbd "SPC d T") 'edebug-Trace-fast-mode          ; trace (capital T)

  ;; Breakpoints
  (kbd "SPC d b") 'edebug-set-breakpoint           ; set/unset breakpoint
  (kbd "SPC d u") 'edebug-unset-breakpoint         ; unset breakpoint
  (kbd "SPC d x") 'edebug-set-conditional-breakpoint ; conditional breakpoint
  (kbd "SPC d X") 'edebug-set-global-break-condition ; global break condition

  ;; Evaluation and inspection
  (kbd "SPC d e") 'edebug-eval-expression          ; eval expression
  (kbd "SPC d C-e") 'edebug-eval-last-sexp         ; eval last sexp
  (kbd "SPC d E") 'edebug-visit-eval-list          ; visit eval list

  ;; Navigation and display
  (kbd "SPC d w") 'edebug-where                    ; where am I
  (kbd "SPC d p") 'edebug-bounce-point             ; show point in source
  (kbd "SPC d P") 'edebug-view-outside             ; view outside windows
  (kbd "SPC d W") 'edebug-toggle-save-windows      ; toggle save windows
  (kbd "SPC d v") 'edebug-visit-eval-list          ; visit eval list

  ;; Control and exit
  (kbd "SPC d Q") 'top-level                       ; quit (abort)
  (kbd "SPC d q") 'edebug-top-level-nonstop        ; quit nonstop
  (kbd "SPC d a") 'abort-recursive-edit            ; abort
  (kbd "SPC d S") 'edebug-stop                     ; stop

  ;; Backtrace and help
  (kbd "SPC d d") 'edebug-pop-to-backtrace         ; show backtrace
  (kbd "SPC d =") 'edebug-temp-display-freq-count  ; show frequency count
  (kbd "SPC d ?") 'edebug-help                     ; help
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
