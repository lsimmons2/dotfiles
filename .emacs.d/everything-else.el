
(message "hola here in everything-else.el")

(setenv "PATH" (concat (getenv "PATH") ":/Users/leo/.ghcup/bin"))
(setq exec-path (append exec-path '("/Users/leo/.ghcup/bin")))

;;TEMPORARY:
(require 'evil)
(evil-mode 1)


(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)

(defun my-disable-evil-C-c-binding ()
  "Disable `C-c` binding in `evil-insert-state-map` for term and shell modes."
  (when (derived-mode-p 'term-mode 'shell-mode)
    (define-key evil-insert-state-map (kbd "C-c") nil)))

(add-hook 'term-mode-hook #'my-disable-evil-C-c-binding)
(add-hook 'shell-mode-hook #'my-disable-evil-C-c-binding)



(evil-define-key 'normal 'global (kbd "S") 'save-buffer)
(setq evil-leader/leader "SPC")

(require 'key-chord)
(key-chord-mode 1)

(defun my-conditional-exit-insert-mode ()
  "Exit Evil insert mode with 'jk' globally, except in term-mode."
  (message "in my-conditional-exit-insert-mode")
  (interactive)
  (unless (derived-mode-p 'term-mode)
    (evil-normal-state)))

(key-chord-define evil-insert-state-map "jk" 'my-conditional-exit-insert-mode)
(key-chord-define evil-insert-state-map "kj" 'my-conditional-exit-insert-mode)
(key-chord-define evil-insert-state-map "JK" 'my-conditional-exit-insert-mode)
(key-chord-define evil-insert-state-map "KJ" 'my-conditional-exit-insert-mode)


(setq key-chord-two-keys-delay 0.2)  ;; Adjust the delay as desired


(defun lsp-next-diagnostic-wrapper ()
  "Move to the next LSP diagnostic."
  (interactive)
  (lsp-next-diagnostic))

(defun lsp-previous-diagnostic-wrapper ()
  "Move to the previous LSP diagnostic."
  (interactive)
  (lsp-previous-diagnostic))

;;LSP mappings
(key-chord-define-global "gd" 'lsp-find-definition)
(key-chord-define-global "gt" 'lsp-find-type-definition)
(key-chord-define-global "]e" 'flycheck-next-error)
(key-chord-define-global "[e" 'flycheck-previous-error)
(key-chord-define-global "gr" 'lsp-find-references)
(key-chord-define-global " r" 'lsp-rename)

(defun my-xref-jump-to-location ()
  "Jump to the location under the cursor in the *xref* buffer."
  (interactive)
  (let ((xref-window (selected-window)))
    (xref-quit-and-goto-xref)
    (select-window xref-window)))

(with-eval-after-load 'evil
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") 'my-xref-jump-to-location))


;; Split windows and move cursor to the new window
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

;; Resize windows
(define-key evil-normal-state-map (kbd "<S-left>") 'shrink-window-horizontally)
(define-key evil-normal-state-map (kbd "<S-right>") 'enlarge-window-horizontally)
(define-key evil-normal-state-map (kbd "<S-up>") 'enlarge-window)
(define-key evil-normal-state-map (kbd "<S-down>") 'shrink-window)

;; Window navigation
(define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)


(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")


;; tabs
(tab-bar-mode 1)
(global-set-key (kbd "M-t") 'tab-new)
(global-set-key (kbd "M-[") 'tab-previous)
(global-set-key (kbd "M-]") 'tab-next)
(global-set-key (kbd "M-w") 'tab-close)



(setq global-auto-revert-non-file-buffers t)  ;; Auto-refresh for non-file buffers (like dired)
(setq auto-revert-verbose nil)                ;; Suppress messages about reverting
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Enable auto-revert in dired-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<") 'dired-up-directory))

(require 'lsp-mode)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)  ;; Enable LSP in Haskell files
(add-hook 'haskell-literate-mode-hook #'lsp)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; Configure lsp-ui to show diagnostics inline
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-update-mode 'line)
(setq lsp-ui-doc-enable t) ;; Enable hover documentation

;;(require 'flycheck)
;;(global-flycheck-mode)
;;(setq lsp-diagnostics-provider :flycheck)

(setq lsp-log-io t)

;;(require 'company)
;;(add-hook 'after-init-hook 'global-company-mode)
;;(setq company-backends '((company-capf company-dabbrev-code))) ;; CAPF is the backend lsp-mode uses


(require 'ns-auto-titlebar)
(ns-auto-titlebar-mode)


;; Apply `my-text-mode-settings` to `text-mode`
(add-hook 'text-mode-hook 'my-text-mode-settings)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "n") 'evil-search-previous)
  (define-key evil-normal-state-map (kbd "N") 'evil-search-next))



(defun open-term-split-below ()
  "Split window below, open a terminal, and focus it."
  (interactive)
  (split-window-below)
  (other-window 1)
  (term "/bin/zsh")) ;; Replace "/bin/bash" with your preferred shell if needed

(defun open-term-split-right ()
  "Split window to the right, open a terminal, and focus it."
  (interactive)
  (split-window-right)
  (other-window 1)
  (term "/bin/zsh")) ;; Replace "/bin/bash" with your preferred shell if needed

(global-set-key (kbd "M-o") 'open-term-split-below)
(global-set-key (kbd "M-e") 'open-term-split-right)


(defun my-term-enter-char-mode ()
  "Switch to term char mode and enter Evil insert mode if in term line mode."
  (interactive)
  (when (and (derived-mode-p 'term-mode) (not (term-in-char-mode)))
    (term-char-mode)
    (evil-insert-state)
    (message "Entering term char mode - switching to Evil insert mode")))

(defun my-term-enter-line-mode ()
  "Enter Evil normal mode when entering term line mode."
  (when (eq major-mode 'term-mode)
    (message "Entering term line mode - switching to Evil normal mode")
    (evil-normal-state)))

(defun my-term-escape-to-line-mode ()
  "Switch from term char mode to term line mode and enter Evil normal state."
  (interactive)
  (term-line-mode)
  (evil-normal-state)
  (message "Switched to term line mode and Evil normal state."))

(defun my-unified-jk-handler ()
  "Handle 'jk' sequence differently depending on the current mode."
  (interactive)
  ;; if in term mode and char mode
  (if (and (derived-mode-p 'term-mode) (term-in-char-mode))
      (my-term-escape-to-line-mode)
    ;; In other modes, just exit Evil insert mode
    (evil-normal-state)))

;; Set up the global "jk" binding to call my-unified-jk-handler in insert mode
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(key-chord-define evil-insert-state-map "jk" 'my-unified-jk-handler)
(key-chord-define evil-insert-state-map "kj" 'my-unified-jk-handler)
(key-chord-define evil-insert-state-map "JK" 'my-unified-jk-handler)
(key-chord-define evil-insert-state-map "KJ" 'my-unified-jk-handler)

(add-hook 'term-mode-hook
          (lambda ()
			(define-key term-raw-map (kbd "C-c") 'term-interrupt-subjob)
			(define-key term-raw-map (kbd "C-d") 'term-send-eof)
            (evil-define-key 'normal term-mode-map (kbd "i") 'my-term-enter-char-mode)
            (evil-define-key 'normal term-mode-map (kbd "a") 'my-term-enter-char-mode)
            (evil-define-key 'insert term-raw-map (kbd "<escape>") 'my-term-escape-to-line-mode)
			(key-chord-define evil-insert-state-map "jk" 'my-unified-jk-handler)
			(key-chord-define evil-insert-state-map "kj" 'my-unified-jk-handler)
			(key-chord-define evil-insert-state-map "JK" 'my-unified-jk-handler)
            (key-chord-define evil-insert-state-map "KJ" 'my-unified-jk-handler)))

(add-hook 'term-char-mode-hook 'evil-insert-state)
(add-hook 'term-line-mode-hook 'evil-normal-state)
;; Add advice to call these functions whenever switching modes
(advice-add 'term-char-mode :after 'my-term-enter-char-mode)
(advice-add 'term-line-mode :after 'my-term-enter-line-mode)



