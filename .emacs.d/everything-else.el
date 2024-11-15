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

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "sop") 'insert-print-statement))

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


(defun open-term-split-below ()
  "Split window below, open a terminal, and focus it."
  (interactive)
  (split-window-below)
  (other-window 1)
  (term "/bin/zsh"))

(defun open-term-split-right ()
  "Split window to the right, open a terminal, and focus it."
  (interactive)
  (split-window-right)
  (other-window 1)
  (term "/bin/zsh"))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "M-o") 'open-term-split-below
    (kbd "M-e") 'open-term-split-right))

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
  (key-chord-define evil-normal-state-map "gd" 'lsp-find-definition)
  (key-chord-define evil-normal-state-map "gt" 'lsp-find-type-definition)
  (key-chord-define evil-normal-state-map "gr" 'lsp-find-references)
  (key-chord-define evil-normal-state-map " r" 'lsp-rename)
  (key-chord-define evil-normal-state-map "]e" 'flycheck-next-error)
  (key-chord-define evil-normal-state-map "[e" 'flycheck-previous-error))

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
