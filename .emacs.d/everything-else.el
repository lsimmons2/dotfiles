(message "hola here in everything-else.el")


;; *****************************
;;VANILLA/VANILLA EVIL MAPPINGS
;; *****************************


					;TODO: maybe I should be using snippets for this
(defvar print-statement-mapping
  '((python-mode . "print()")
    (typescript-mode . "console.log();")
    (typescript-ts-mode . "console.log();")
    (tsx-ts-mode . "console.log();")
    (js-mode . "console.log();"))
  "Mapping of major modes to their respective print statements.")

(defun insert-print-statement ()
  "Insert a print statement depending on the current major mode."
  (interactive)
  (message "Current major mode: %s" major-mode) ;; Debugging message
  (let ((print-statement (cdr (assoc major-mode print-statement-mapping))))
    (if print-statement
        (progn
          (insert print-statement)
          (backward-char (if (string-suffix-p "();" print-statement) 2 1))) ;; Adjust cursor position
      (message "No print statement template for this mode."))))

					;(with-eval-after-load 'evil
					;(define-key evil-insert-state-map (kbd "sop") 'insert-print-statement))

					;if runs * while hovering over FOO_BAR, will search FOO_BAR instead of just FOO or BAR
(setq evil-symbol-word-search t)


(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    ;; Open the project root in dired
    (kbd "SPC e") (lambda () (interactive)
                    (let ((project-root (or (projectile-project-root) default-directory)))
                      (find-file project-root)))
    ;; Open the current buffer's directory in dired
    (kbd "SPC w") (lambda () (interactive)
                    (find-file (file-name-directory (or buffer-file-name default-directory))))))

					;get some vim stuff in dired
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; Use Evil's search module in dired
              (setq evil-search-module 'evil-search)
              ;; Define keybindings for search
              (evil-define-key 'normal dired-mode-map
                (kbd "/") 'evil-search-forward  ;; Search with /
                (kbd "n") 'evil-search-next     ;; Navigate to next match
                (kbd "N") 'evil-search-previous ;; Navigate to previous match
                (kbd "%") 'dired-create-empty-file
                (kbd "gg") 'evil-goto-first-line ;; Go to the top of the buffer
                (kbd "G") 'evil-goto-line))))    ;; Go to the bottom of the buffer


(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-r") 'undo-redo)) ;; Redo

(tab-bar-mode 1)
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "M-t") 'tab-new
    (kbd "M-[") 'tab-previous
    (kbd "M-]") 'tab-next
    (kbd "M-w") 'tab-close)
  )


(defun save-all-buffers ()
  "Save all modified file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t (lambda () buffer-file-name))) ;; Only consider file-visiting buffers

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "S") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "SPC S") 'save-all-buffers))

(evil-define-key 'normal 'global (kbd "SPC s h") 
  (lambda () (interactive) (split-window-right) (other-window 1)))
(evil-define-key 'normal 'global (kbd "SPC s l") 
  (lambda () (interactive) (split-window-right) (other-window 1)))
(evil-define-key 'normal 'global (kbd "SPC s k") 
  (lambda () (interactive) (split-window-below) (other-window 1)))
(evil-define-key 'normal 'global (kbd "SPC s j") 
  (lambda () (interactive) (split-window-below) (other-window 1)))

(evil-define-key 'normal 'global (kbd "SPC o l")
  (lambda ()
    (interactive)
    (display-line-numbers-mode (if display-line-numbers-mode -1 1))))

					;TODO: this doesn't behave like I want it to/like native hls in vim
(defun toggle-evil-search-highlight ()
  "Toggle persistent search highlight for evil-mode, restoring highlights if re-enabled."
  (interactive)
  (if (bound-and-true-p global-evil-search-highlight-persist)
      (progn
        (evil-search-highlight-persist-remove-all)  ;; Clear all highlights
        (global-evil-search-highlight-persist -1))  ;; Turn off persistent highlighting
    (progn
      (global-evil-search-highlight-persist t)     ;; Turn on persistent highlighting
      ;; Re-apply the last search pattern
      (when evil-ex-search-pattern
        (evil-ex-search-activate-highlight evil-ex-search-pattern))
      )))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "SPC o h") 'toggle-evil-search-highlight))

(defun my-toggle-comment ()
  "Toggle comments on the current line or region in normal, visual, or visual-block mode."
  (interactive)
  (if (use-region-p)  ;; If a region is selected
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; Bind SPC c for commenting
(with-eval-after-load 'evil
  (evil-define-key 'normal global-map (kbd "SPC c") 'my-toggle-comment)
  (evil-define-key 'visual global-map (kbd "SPC c") 'my-toggle-comment))

(with-eval-after-load 'evil
  ;; Window resizing
  (evil-define-key 'normal 'global
    (kbd "<S-left>") 'shrink-window-horizontally
    (kbd "<S-right>") 'enlarge-window-horizontally
    (kbd "<S-up>") 'enlarge-window
    (kbd "<S-down>") 'shrink-window)

  ;; Window navigation
  (evil-define-key 'normal 'global
    (kbd "C-j") 'windmove-down
    (kbd "C-k") 'windmove-up
    (kbd "C-l") 'windmove-right
    (kbd "C-h") 'windmove-left
    (kbd "M-j") 'windmove-down
    (kbd "M-k") 'windmove-up
    (kbd "M-l") 'windmove-right
    (kbd "M-h") 'windmove-left))


(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "n") 'evil-search-previous
    (kbd "N") 'evil-search-next))


					;NB: still probably some redundancy and room for improvement with all this
					;exit and enter insert/term-char mode stuff
(defun my-term-enter-char-mode ()
  "Switch to term char mode and enter Evil insert mode if in term line mode."
  (interactive)
  (when (and (derived-mode-p 'term-mode) (not (term-in-char-mode)))
    (term-char-mode)
    (evil-insert-state)
    (message "Entering term char mode - switching to Evil insert mode")))

(defun my-term-enter-line-mode ()
  "Switch from term char mode to term line mode and enter Evil normal state."
  (interactive)
  (term-line-mode)
  (evil-normal-state)
  (message "Switched to term line mode and Evil normal state."))

(defun my-universal-exit-insert-mode ()
  "Handle 'jk' sequence differently depending on the current mode."
  (interactive)
  ;; if in term mode and char mode
  (if (and (derived-mode-p 'term-mode) (term-in-char-mode))
      (my-term-enter-line-mode)
    ;; In other modes, just exit Evil insert mode
    (evil-normal-state)))

(key-chord-define evil-insert-state-map "jk" 'my-universal-exit-insert-mode)
(key-chord-define evil-insert-state-map "kj" 'my-universal-exit-insert-mode)
(key-chord-define evil-insert-state-map "JK" 'my-universal-exit-insert-mode)
(key-chord-define evil-insert-state-map "KJ" 'my-universal-exit-insert-mode)

(add-hook 'term-char-mode-hook 'evil-insert-state)
(add-hook 'term-line-mode-hook 'evil-normal-state)

(add-hook 'term-mode-hook
          (lambda ()
            (evil-define-key 'insert term-raw-map (kbd "C-c") 'term-interrupt-subjob)
            (evil-define-key 'insert term-raw-map (kbd "C-d") 'term-send-eof)
            (evil-define-key 'normal term-mode-map (kbd "i") 'my-term-enter-char-mode)
            (evil-define-key 'normal term-mode-map (kbd "a") 'my-term-enter-char-mode)
            (evil-define-key 'insert term-raw-map (kbd "<escape>") 'my-term-enter-line-mode)
            ))


;; *****************************
;;MAPPINGS RE HELM, LSP, COMPANY, FLYCHECK
;; *****************************

(with-eval-after-load 'helm
  ;; Bind TAB to expand without opening the action menu
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; Custom keybindings for navigating Helm lists
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line))

(with-eval-after-load 'helm-buffers
  (define-key helm-buffer-map (kbd "DEL") 'helm-buffer-run-kill-buffers))

;; Global Helm bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-p") 'project-find-file)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(defun helm-projectile-switch-project-dired ()
  "Switch to a recent project and open it in Dired."
  (interactive)
  (let ((projectile-switch-project-action #'projectile-dired))
    (helm-projectile-switch-project)))
(global-set-key (kbd "C-x C-d") 'helm-projectile-switch-project-dired)

(with-eval-after-load 'evil
  ;; General LSP mappings
  (key-chord-define evil-normal-state-map "gd" 'lsp-find-definition)
  (key-chord-define evil-normal-state-map "gt" 'lsp-find-type-definition)
  (key-chord-define evil-normal-state-map "gr" 'lsp-find-references)
  (key-chord-define evil-normal-state-map " r" 'lsp-rename)
  (key-chord-define evil-normal-state-map "]e" 'flycheck-next-error)
  (key-chord-define evil-normal-state-map "[e" 'flycheck-previous-error)

  ;; OCaml-specific overrides using Merlin
  (add-hook 'tuareg-mode-hook
            (lambda ()
              ;; Use Merlin instead of LSP for definition and type navigation
              (key-chord-define evil-normal-state-local-map "gd" 'merlin-locate)
              (key-chord-define evil-normal-state-local-map "gt" 'merlin-type-enclosing)
              (key-chord-define evil-normal-state-local-map "gr" 'merlin-project-occurrences))))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))

;;for going to references in list provided by lsp/helm
(defun my-xref-jump-to-location ()
  "Jump to the location under the cursor in the *xref* buffer."
  (interactive)
  (let ((xref-window (selected-window)))
    (xref-quit-and-goto-xref)
    (select-window xref-window)))
(with-eval-after-load 'evil
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") 'my-xref-jump-to-location))

;;jump back (../) in dired with <
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<") 'dired-up-directory))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)                  ;; Suppress messages about reverting
(setq global-auto-revert-non-file-buffers t)    ;; Enable auto-revert for dired buffers (and others?)


(defun open-new-term ()
  "Open a new terminal buffer with a unique name and process."
  (interactive)
  (let ((term-buffer (generate-new-buffer-name "*term*")))
    (with-current-buffer (term "/bin/zsh")  ;; Replace "/bin/bash" with your preferred shell
      (rename-buffer term-buffer))))


					;search all lines of project
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "SPC /") 'helm-projectile-rg)
  (evil-define-key 'normal 'global (kbd "SPC ?") 'helm-projectile-rg)
					;TODO: this isn't working 
					;(evil-define-key 'normal 'global (kbd "/") 'helm-occur)
  )


(defun my-helm-projectile-rg-clear-input (orig-fun &rest args)
  "Clear default input for helm-projectile-rg."
  (let ((helm-ag-insert-at-point 'symbol)) ;; Set to nil to prevent default
    (apply orig-fun args)))

(advice-add 'helm-projectile-rg :around #'my-helm-projectile-rg-clear-input)



(defun my-text-mode-setup ()
  "Custom settings for text-mode."
  ;; Set tabs instead of spaces, and configure tab width
  (setq-local indent-tabs-mode t)   ;; Use tabs instead of spaces
  (setq-local tab-width 4)          ;; Set tab width to 4
  (setq-local evil-shift-width 4)   ;; Set shift width to 4 (for Evil mode indentation)

  ;; Folding
  (setq-local outline-regexp "^[ \t]*") ;; Use indentation for folding
  (outline-minor-mode 1)                ;; Enable folding for text-mode

  ;; Wrapping and indentation
  (setq-local word-wrap t)               ;; Enable word wrapping
  (setq-local truncate-lines nil)        ;; Prevent horizontal scrolling
  (setq-local visual-line-mode t)        ;; Enable visual line wrapping

  ;; Enable adaptive-wrap-prefix-mode for wrapped lines
  (adaptive-wrap-prefix-mode 1)
  ;(setq-local adaptive-wrap-extra-indent 4) ;; Match the indentation of wrapped lines with the original line

  ;; Keybinding equivalent for `<leader>j` in Vim
  (evil-define-key 'normal text-mode-map
    (kbd "SPC j") (lambda () (interactive)
                    (evil-visual-line)
                    (evil-upcase)
                    (evil-insert-line)
                    (insert ">")
                    (evil-normal-state)))
)

(add-hook 'text-mode-hook #'my-text-mode-setup)



;; Enable visual-line-mode globally
(global-visual-line-mode 1)

;; Make 'j' and 'k' move by visual lines in Evil mode
(defun my-evil-visual-line-navigation ()
  "Make `j` and `k` move by visual lines instead of logical lines."
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line))

;; Apply the navigation adjustment after Evil mode loads
(with-eval-after-load 'evil
  (my-evil-visual-line-navigation))


(with-eval-after-load 'evil
  ;; Bind `C-u` to scroll up
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  ;; Bind `C-d` to scroll down (default in Evil, but ensure it's consistent)
  ;(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  ;(define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
  )

(defun my-enable-superword-mode ()
  "Enable superword-mode for treating underscores as part of words."
  (superword-mode 1))

;; Enable superword-mode for all programming modes (or specific ones)
(add-hook 'prog-mode-hook #'my-enable-superword-mode)

;; Optional: Enable it for text-mode too if needed
(add-hook 'text-mode-hook #'my-enable-superword-mode)


(defun my-evil-superword-setup ()
  "Customize Evil to work with `superword-mode`."
  (when (bound-and-true-p superword-mode)
    ;; Use symbol motions to treat underscores as part of words
    (define-key evil-motion-state-map (kbd "w") 'evil-forward-symbol-begin)
    (define-key evil-motion-state-map (kbd "e") 'evil-forward-symbol-end)
    (define-key evil-motion-state-map (kbd "b") 'evil-backward-symbol-begin)))

(defun evil-forward-symbol-begin ()
  "Move forward to the beginning of the next symbol."
  (interactive)
  (forward-symbol 1))

(defun evil-backward-symbol-begin ()
  "Move backward to the beginning of the previous symbol."
  (interactive)
  (forward-symbol -1))

(defun evil-forward-symbol-end ()
  "Move forward to the end of the current symbol."
  (interactive)
  (let ((start (point)))
    (forward-symbol 1)
    (if (= start (point)) ; If no movement happened, we're at the end of the buffer
        (message "Already at the end of the buffer")
      (backward-char)
      ;; Ensure we're at the last character of the symbol
      (while (looking-at "[a-zA-Z0-9_]") 
        (forward-char)))))

;; Enable this behavior for modes with `superword-mode`
(add-hook 'superword-mode-hook #'my-evil-superword-setup)




(defvar my-fullscreen-window nil
  "Stores the window configuration before toggling fullscreen.")

(defun my-toggle-window-fullscreen ()
  "Toggle the selected window to take up the entire frame."
  (interactive)
  (if my-fullscreen-window
      ;; Restore previous window configuration
      (progn
        (set-window-configuration my-fullscreen-window)
        (setq my-fullscreen-window nil))
    ;; Save current window configuration and maximize the selected window
    (progn
      (setq my-fullscreen-window (current-window-configuration))
      (delete-other-windows))))

;; Bind the toggle functionality to C-9
(global-set-key (kbd "C-9") 'my-toggle-window-fullscreen)
