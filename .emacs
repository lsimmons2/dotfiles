(require 'mouse)
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda ()
                           (interactive)
                           (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                           (interactive)
                           (scroll-up 1)))

(setq mouse-sel-mode t)
(defun track-mouse (e))


;; one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; keyboard scroll one line at a time
(setq scroll-step 1)

;; autosave and backups
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backup-list")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/auto-save-list")))

;; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)