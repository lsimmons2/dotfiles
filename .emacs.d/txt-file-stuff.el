
(defun my-text-mode-setup ()
  "Custom settings for text-mode."
  ;; Set tabs instead of spaces, and configure tab width
  (setq-local indent-tabs-mode t)   ;; Use tabs instead of spaces
  (setq-local tab-width 4)          ;; Set tab width to 4
  (setq-local evil-shift-width 4)   ;; Set shift width to 4 (for Evil mode indentation)

  ;; Folding
  (setq-local outline-regexp "^[ \t]*[^ \t\n]") ;; Match only non-blank lines with leading spaces
  (setq-local outline-blank-line t)             ;; Prevent blank lines from being part of the fold
  (outline-minor-mode 1)                       ;; Enable outline minor mode


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


  (add-hook 'outline-view-change-hook
            (lambda ()
              (dolist (ov (overlays-in (point-min) (point-max)))
                (when (eq (overlay-get ov 'invisible) 'outline)
                  (overlay-put ov 'display
                               (propertize " <folded>"
                                           'face 'font-lock-string-face)))))
            nil t)) ;; Use buffer-local hook

(add-hook 'text-mode-hook #'my-text-mode-setup)
