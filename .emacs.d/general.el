
(use-package company
  :disabled t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package evil
  :ensure t
  :config
  (setq evil-undo-system 'undo-redo) ;; Use the default Emacs undo system with redo support
  (evil-mode 1))


(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode))


(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2))

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-boring-file-regexp-list
        '("\\.git$"
          "node_modules"
          "target"
          "dist")))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)              ;; Enable caching for faster performance
  (setq projectile-globally-ignored-directories '(".git" "node_modules" "dist"))
  (projectile-mode +1)
  (add-to-list 'projectile-ignored-projects "~/")
  :custom
  (projectile-completion-system 'helm)
  )


(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package evil-search-highlight-persist
  :ensure t
  :config
  (global-evil-search-highlight-persist t)) ;; Enable persistent highlights globally


(use-package adaptive-wrap
  :ensure t)


(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode) ;; Enable in programming modes
  :config
  (setq highlight-symbol-idle-delay 0.3)) ;; Highlight after 0.3 seconds


(setq scroll-margin 1)                   ;; Keeps a margin of 1 line at the top/bottom
(setq scroll-conservatively 101)         ;; Scroll only by one line to avoid jumping
(setq scroll-step 1)                     ;; Scrolls one line at a time
(setq auto-window-vscroll nil)           ;; Disables auto vertical scroll jump
(setq redisplay-dont-pause t)            ;; Prevents pauses in redisplay, smoothing scrolling


(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t)))



(defun reload-emacs-config ()
  "Reload Emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Emacs configuration reloaded!"))


(defun open-term-split-below ()
  "Split window below, open a new terminal session, and focus it."
  (interactive)
  (let ((new-term-name (generate-new-buffer-name "term")))
    (message "new term name be %s" new-term-name)
    (split-window-below)
    (other-window 1)
    (term "/bin/zsh")
    (rename-buffer new-term-name)))

(defun open-term-split-right ()
  "Split window to the right, open a new terminal session, and focus it."
  (interactive)
  (let ((new-term-name (generate-new-buffer-name "term")))
    (split-window-right)
    (other-window 1)
    (term "/bin/zsh")
    (rename-buffer new-term-name)))


(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "M-o") 'open-term-split-below
    (kbd "M-e") 'open-term-split-right))


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

(defun my-dired-create-and-open-file (filename)
  "Create an empty file and open it in the current buffer."
  ;; F in FCreate for getting emacs to prompt for file name correctly
  (interactive "FCreate empty file: ")
  (dired-create-empty-file filename)
  (find-file filename)) ;; Open the file in the current buffer


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
                (kbd "%") 'my-dired-create-and-open-file
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

(defun split-window-left ()
  "Split the current window and create a new one on the left."
  (interactive)
  (split-window-right)       ;; Create a new window to the right
  (let ((new-window (next-window))) ;; Get the newly created window
    (other-window 1)         ;; Switch to the new window
    (set-window-buffer new-window (current-buffer)) ;; Swap buffers
    (other-window 1)         ;; Move focus to the left window
    (balance-windows)))      ;; Balance all windows

(evil-define-key 'normal 'global (kbd "SPC s h") 'split-window-left)

(evil-define-key 'normal 'global (kbd "SPC s l") 
  (lambda ()
    (interactive)
    (split-window-right)
    (other-window 1)
    (balance-windows)))

(defun split-window-above ()
  "Split the current window and create a new one above."
  (interactive)
  (split-window-below)       ;; Create a new window below
  (let ((new-window (next-window))) ;; Get the newly created window
    (other-window 1)         ;; Switch to the new window
    (set-window-buffer new-window (current-buffer)) ;; Swap buffers
    (other-window 1)         ;; Move focus to the above window
    (balance-windows)))      ;; Balance all windows

(evil-define-key 'normal 'global (kbd "SPC s k")  'split-window-above)

(evil-define-key 'normal 'global (kbd "SPC s j") 
  (lambda ()
    (interactive)
    (split-window-below)
    (other-window 1)
    (balance-windows)))


;; window splitting mappings for dired buffers
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "SPC s h") 'split-window-left
    (kbd "SPC s l") (lambda ()
                      (interactive)
                      (split-window-right)
                      (other-window 1)
                      (balance-windows))
    (kbd "SPC s k") 'split-window-above
    (kbd "SPC s j") (lambda ()
                      (interactive)
                      (split-window-below)
                      (other-window 1)
                      (balance-windows))))



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


(with-eval-after-load 'term
  (add-hook 'term-mode-hook
            (lambda ()
              ;; Prevent Evil from interfering in term-char-mode
              (evil-define-key 'insert term-raw-map (kbd "C-r") 'term-send-raw)
              (evil-define-key 'insert term-raw-map (kbd "C-p") 'term-send-up)
              (evil-define-key 'insert term-raw-map (kbd "C-n") 'term-send-down))))

(defun my-term-enter-char-mode ()
  "Switch to term char mode and enter Evil insert mode if in term line mode."
  (interactive)
  (when (and (derived-mode-p 'term-mode) (not (term-in-char-mode)))
    (term-char-mode)
    (evil-insert-state)
    (setq-local dabbrev-expand nil)
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
            (evil-define-key 'normal term-mode-map (kbd "A") 'my-term-enter-char-mode)
            (evil-define-key 'insert term-raw-map (kbd "<escape>") 'my-term-enter-line-mode)
            ))



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

(defun my/helm-projectile-dired ()
  "Open a Helm list of projects and open Dired in the selected project's root."
  (interactive)
  (require 'helm-projectile)
  (helm :sources (helm-build-sync-source "Projectile Projects"
                   :candidates (projectile-relevant-known-projects)
                   :action (lambda (project)
                             (dired (expand-file-name project))))
        :buffer "*helm projectile dired*"))

(defun my/helm-projectile-dired-new-tab ()
  "Open a Helm list of projects and open Dired in the selected project's root in a new tab."
  (interactive)
  (require 'helm-projectile)
  (helm :sources (helm-build-sync-source "Projectile Projects"
                   :candidates (projectile-relevant-known-projects)
                   :action (lambda (project)
                             (let ((default-directory (expand-file-name project)))
                               (tab-bar-new-tab)
                               (dired default-directory))))
        :buffer "*helm projectile dired*"))


(with-eval-after-load 'evil
  ;; Global mappings
  (evil-define-key 'normal 'global (kbd "SPC l") 'my/helm-projectile-dired)
  (evil-define-key 'normal 'global (kbd "SPC L") 'my/helm-projectile-dired-new-tab)

  ;; Dired-specific mappings
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map (kbd "SPC l") 'my/helm-projectile-dired)
    (evil-define-key 'normal dired-mode-map (kbd "SPC L") 'my/helm-projectile-dired-new-tab)))



(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))



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



;; Define a function to append "✓"
(defun my-append-checkmark ()
  (interactive)
  (end-of-line)
  (insert " ✓"))

;; Define a function to append "✗"
(defun my-append-crossmark ()
  (interactive)
  (end-of-line)
  (insert " ✗"))

(defun my-insert-current-date ()
  (interactive)
  (end-of-line)
  (insert (format-time-string "%m.%d.%Y")))

(evil-define-key 'normal global-map
  (kbd "SPC a k") 'my-insert-current-date
  (kbd "SPC a d") 'my-insert-current-date
  (kbd "SPC a c") 'my-append-checkmark
  (kbd "SPC a x") 'my-append-crossmark)
