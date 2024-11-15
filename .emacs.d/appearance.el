
(defvar my/dark-theme 'doom-one)
(defvar my/light-theme 'doom-feather-light)

(set-face-attribute 'default nil :weight 'light)

(defun my/detect-macos-dark-mode ()
  "Check if macOS is in dark mode and return t if it is."
  (string=
   "Dark\n"
   (shell-command-to-string "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light")))


(defun my/toggle-theme-based-on-system ()
  "Toggle between `my/dark-theme` and `my/light-theme` based on macOS appearance.
Only loads the theme if it's not already active, to prevent flickering."
  (if (my/detect-macos-dark-mode)
      (unless (member my/dark-theme custom-enabled-themes)
        (message "System is in dark mode; switching to %s theme." my/dark-theme)
        (load-theme my/dark-theme t))
    (unless (member my/light-theme custom-enabled-themes)
      (message "System is in light mode; switching to %s theme." my/light-theme)
      (load-theme my/light-theme t))))


(defun my/set-dark-light-mode-based-on-system ()
  "Manually toggle between light and dark themes."
  (interactive)
  (my/toggle-theme-based-on-system))


(run-at-time nil (* 1 5) 'my/toggle-theme-based-on-system)


(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


(setq display-buffer-base-action '(display-buffer-same-window))


(defun my-dired-highlight-line ()
  "Highlight the entire line under the cursor in Dired mode."
  (hl-line-mode 1))  ;; Enable hl-line-mode for line highlighting
(add-hook 'dired-mode-hook 'my-dired-highlight-line)




(setq-default mode-line-buffer-identification
              '(:eval (if (and (featurep 'projectile)
                               (projectile-project-root)
                               buffer-file-name)
                          (file-relative-name buffer-file-name (projectile-project-root))
                        "%b")))
;(setq-default mode-line-buffer-identification
              ;'(:eval (if (and (featurep 'projectile) (projectile-project-root))
                          ;(file-relative-name buffer-file-name (projectile-project-root))
                        ;"%b")))

