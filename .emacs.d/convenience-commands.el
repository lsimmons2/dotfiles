

(defun reload-emacs-config ()
  "Reload Emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Emacs configuration reloaded!"))

(defvar counter 0)
(defun my/open-terminal ()
  "Open a new terminal and rename the buffer, focusing the new terminal window."
  (interactive)
  (setq counter (+ counter 1))
  (let* ((title (concat "Terminal-" (number-to-string counter)))
         (buf-title (concat "*" title "*")))
    (message buf-title)
    (switch-to-buffer-other-window buf-title)
    (set-buffer (make-term title "/bin/zsh"))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer buf-title)))



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
