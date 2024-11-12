(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default)) '(package-selected-packages
   '(doom-themes ns-auto-titlebar company flycheck lsp-ui lsp-haskell lsp-mode key-chord evil-leader ## evil haskell-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-bar-tab ((t (:background "gray60" :foreground "black" :weight bold))))
 '(tab-bar-tab-inactive ((t (:background "gray85" :foreground "black")))))

(load-theme 'doom-one t) ;; Automatically loads the 'wombat' theme


(setenv "PATH" (concat (getenv "PATH") ":/Users/leo/.ghcup/bin"))
(setq exec-path (append exec-path '("/Users/leo/.ghcup/bin")))

;;TEMPORARY:
(require 'evil)
(evil-mode 1)          ;; Enable evil-mode globally

;;(find-file "~/.emacs")
(setq initial-buffer-choice "~/.emacs")
;;(setq initial-buffer-choice "~/dev")



(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
;; Function to remove the C-c binding from evil-insert-state-map in term-mode and shell-mode
(defun my-disable-evil-C-c-binding ()
  "Disable `C-c` binding in `evil-insert-state-map` for term and shell modes."
  (when (derived-mode-p 'term-mode 'shell-mode)
    (define-key evil-insert-state-map (kbd "C-c") nil)))

;; Add this function to term-mode-hook and shell-mode-hook
(add-hook 'term-mode-hook #'my-disable-evil-C-c-binding)
(add-hook 'shell-mode-hook #'my-disable-evil-C-c-binding)


(defun my-toggle-term-mode ()
  "Toggle between navigation (term-line-mode) and terminal input (term-char-mode) in term buffers."
  (interactive)
  (message "Toggle term mode function called")  ;; Debug message
  (if (term-in-line-mode)
      (progn
        (term-char-mode)
        (evil-insert-state)
        (message "Switched to term-char-mode and evil-insert-state")) ;; Then-part
    (progn
      (term-line-mode)
      (evil-normal-state)
      (message "Switched to term-line-mode and evil-normal-state")))) ;; Else-part



(evil-define-key 'normal 'global (kbd "S") 'save-buffer)
(setq evil-leader/leader "SPC")

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "JK" 'evil-normal-state)
(key-chord-define evil-insert-state-map "KJ" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kJ" 'evil-normal-state)
(key-chord-define evil-insert-state-map "Kj" 'evil-normal-state)
(setq key-chord-two-keys-delay 0.2)  ;; Adjust the delay as desired


;; Split window bindings
;; Split vertically (right)
(evil-define-key 'normal 'global (kbd "SPC s h") (lambda () (interactive) (split-window-right)))
;; Split vertically (left)
(evil-define-key 'normal 'global (kbd "SPC s l") (lambda () (interactive) (split-window-right) (other-window 1)))
;; Split horizontally (above)
(evil-define-key 'normal 'global (kbd "SPC s k") (lambda () (interactive) (split-window-below) (other-window -1)))
;; Split horizontally (below)
(evil-define-key 'normal 'global (kbd "SPC s j") (lambda () (interactive) (split-window-below)))

;; Resize windows
(define-key evil-normal-state-map (kbd "<S-left>") 'shrink-window-horizontally)
(define-key evil-normal-state-map (kbd "<S-right>") 'enlarge-window-horizontally)
(define-key evil-normal-state-map (kbd "<S-up>") 'enlarge-window)
(define-key evil-normal-state-map (kbd "<S-down>") 'shrink-window)

;; Window navigation
;;(define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
;;(define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
;;(define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
;;(define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)

;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(setq ring-bell-function 'ignore)



(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; tabs
(tab-bar-mode 1)
(global-set-key (kbd "M-t") 'tab-new)
(global-set-key (kbd "M-[") 'tab-previous)
(global-set-key (kbd "M-]") 'tab-next)
(global-set-key (kbd "M-w") 'tab-close)



(setq global-auto-revert-non-file-buffers t)  ;; Auto-refresh for non-file buffers (like dired)
(setq auto-revert-verbose nil)                ;; Suppress messages about reverting
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Enable auto-revert in dired-mode


;; Set backup directory
;;(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;; Set autosave directory
;;(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t)))


(setq scroll-margin 1)                   ;; Keeps a margin of 1 line at the top/bottom
(setq scroll-conservatively 101)         ;; Scroll only by one line to avoid jumping
(setq scroll-step 1)                     ;; Scrolls one line at a time
(setq auto-window-vscroll nil)           ;; Disables auto vertical scroll jump
(setq redisplay-dont-pause t)            ;; Prevents pauses in redisplay, smoothing scrolling


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

(require 'flycheck)
(global-flycheck-mode)
(setq lsp-diagnostics-provider :flycheck)

(setq lsp-log-io t)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends '((company-capf company-dabbrev-code))) ;; CAPF is the backend lsp-mode uses


(require 'ns-auto-titlebar)
(ns-auto-titlebar-mode)


;; Set up text-mode specific settings
(defun my-text-mode-settings ()
  "Custom settings for text files."

  ;; Set tab-related settings
  (setq-local indent-tabs-mode nil)         ;; No tab expansion
  (setq-local tab-width 4)                  ;; Set tab width to 4
  (setq-local evil-shift-width 4)           ;; For Evil mode

  ;; Set fold method to indentation-based (requires hs-minor-mode for folding)
  (setq-local hs-hide-comments-when-hiding-all nil)  ;; Optional for hideshow-mode
  (setq-local hs-minor-mode t)                         ;; Activate folding for text-mode
  (setq-local hs-allow-nesting t)

  ;; Wrap, break indentation, and linebreak settings
  (visual-line-mode 1)                                ;; Enable visual line wrapping
  (setq-local word-wrap t)
  (setq-local line-move-visual t)
  (setq-local truncate-lines nil)                     ;; Ensure lines wrap
  (setq-local adaptive-wrap-extra-indent 2)           ;; Break indentation with 2-space shift
  (setq-local adaptive-wrap-prefix-mode 1)            ;; Activate adaptive wrap

  ;; Define a custom Evil normal mode keybinding (if using Evil mode)
  (when (bound-and-true-p evil-mode)
    (evil-define-key 'normal text-mode-map
      (kbd "<leader>j") "VU<A>>"))                    ;; Equivalent mapping in Evil
)

;; Apply `my-text-mode-settings` to `text-mode`
(add-hook 'text-mode-hook 'my-text-mode-settings)

;; Function to toggle between terminal input mode (term-char-mode) and navigation mode (term-line-mode)
(defun my-toggle-term-mode ()
  "Toggle between navigation mode and terminal input mode in term buffers."
  (interactive)
  
  ;; Check if we are in term-line-mode (for navigation).
  ;; `term-in-line-mode` returns true if we're in line mode, false otherwise.
  (if (term-in-line-mode)
      
      ;; If we are in line mode, switch to char mode for direct terminal input.
      ;; This is similar to being in a normal terminal where keybindings like
      ;; C-p, C-n, and others work directly.
      (progn
        (term-char-mode)        ;; Switch to term-char-mode (terminal input mode)
        (evil-insert-state))    ;; Switch to Evil's Insert state for direct input

    ;; If we are in char mode, switch to line mode for navigation.
    ;; This allows us to move around the output using `hjkl` and use
    ;; other Evil commands or Emacs commands.
    (term-line-mode)            ;; Switch to term-line-mode (navigation mode)
    (evil-normal-state)))       ;; Switch to Evil's Normal state for navigation



(message "hola mundo - corrio ~/.emacs!!")
